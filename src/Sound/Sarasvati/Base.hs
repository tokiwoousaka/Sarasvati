{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Sound.Sarasvati.Base (
  SarasvatiConfig(..),
  defaultConfig,
  sarasvatiOutput
  ) where
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Monoid
import Sound.PortAudio 
import Foreign.C.Types (CFloat(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, pokeElemOff)

----------------
-- type definition

-- api configration
data SarasvatiConfig = SarasvatiConfig {
  confSampleRate :: Double,
  confFramesPerBuffer :: Int
  }

defaultConfig :: SarasvatiConfig
defaultConfig = SarasvatiConfig {
  confSampleRate = 44100,
  confFramesPerBuffer = 3000
  }

-- helper
data StreamState = Running | Finished deriving (Show, Eq, Read)

----------------
-- api

sarasvatiOutput :: SarasvatiConfig -> [(Float, Float)] -> IO (Either Error ())
sarasvatiOutput conf lst = withPortAudio $ runSarasvatiOutput conf (conv2CFloat lst)

runSarasvatiOutput :: SarasvatiConfig -> [(CFloat, CFloat)] -> IO (Either Error ())
runSarasvatiOutput conf lst = do
  -- env
  mstat <- newMVar Running
  tbl <- newMVar lst
  -- config
  smprate <- return $ confSampleRate conf
  fpb <- return . Just $ confFramesPerBuffer conf
  -- call back
  final <- return (Just $ swapMVar mstat Finished >> return ())
  withDefaultStream 0 2 smprate fpb (Just $ outputCallback conf tbl) final $ streaming mstat

    where 
      streaming :: MVar StreamState -> Stream CFloat CFloat -> IO (Either Error ())
      streaming stat strm = do
        st <- readMVar stat
        startStream strm 
        if st == Finished 
          then stopStream strm >> return (Right ())
          else streaming stat strm

----------------
-- callback function

outputAction :: Ptr CFloat -> (CFloat, CFloat) -> Int -> IO ()
outputAction out (v1, v2) i = do
  pokeElemOff out (2 * i) v1
  pokeElemOff out (2 * i + 1) v2

runOutput :: Ptr CFloat -> [(CFloat, CFloat)] -> IO ()
runOutput out lst = mapM_ (uncurry $ outputAction out) $ zip lst [1..]

outputCallback :: SarasvatiConfig -> MVar [(CFloat, CFloat)] -> StreamCallback CFloat CFloat
outputCallback conf mvar _ _ frames _ out = do
  let frameLen = fromIntegral $ frames - 1
  -- read list data
  list <- readMVar mvar
  (target, next) <- return $ splitAt frameLen list
  -- write to output pointer
  runOutput out . take frameLen $ target ++ (repeat (0, 0)) 

  -- swap user data
  swapMVar mvar next 
  -- result value
  return $ if next == [] then Complete else Continue

----------------
-- hepler

conv2CFloat :: [(Float, Float)] -> [(CFloat, CFloat)]
conv2CFloat [] = []
conv2CFloat ((v1,v2):xs) = (CFloat v1, CFloat v2) : conv2CFloat xs

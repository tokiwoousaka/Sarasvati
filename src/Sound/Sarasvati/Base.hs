{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Sound.Sarasvati.Base (
  SarasvatiConfig(..),
  Channel(..),
  ChannelInfo(..),
  defaultConfig,
  sarasvatiOutput
  ) where
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Sound.PortAudio 
import Foreign.C.Types (CFloat(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, pokeElemOff)

----------------
-- configration 

data SarasvatiConfig = SarasvatiConfig {
  confSampleRate :: Double,
  confFramesPerBuffer :: Int
  }

defaultConfig :: SarasvatiConfig
defaultConfig = SarasvatiConfig {
  confSampleRate = 44100,
  confFramesPerBuffer = 5000
  }

----------------
-- api

sarasvatiOutput :: Channel c h => SarasvatiConfig -> [c] -> IO (Either Error ())
sarasvatiOutput conf lst = 
  withPortAudio $ runSarasvatiOutput conf (map toChannelInfo lst)

runSarasvatiOutput :: ChannelInfo c => SarasvatiConfig -> [c] -> IO (Either Error ())
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
-- channel

class (Eq c , Eq h, ChannelInfo h) => Channel c h | c -> h where
  toChannelInfo :: c -> h
class Eq c => ChannelInfo c where
  outAction :: Ptr CFloat -> c -> Int -> IO ()

-- stereo

instance Channel (Float, Float) (CFloat, CFloat) where
  toChannelInfo (v1,v2) = (CFloat v1, CFloat v2)
instance ChannelInfo (CFloat, CFloat) where
  outAction out (v1, v2) i = do
    pokeElemOff out (2 * i) v1
    pokeElemOff out (2 * i + 1) v2

----------------
-- callback function

runOutput :: ChannelInfo c => Ptr CFloat -> [c] -> IO ()
runOutput out lst = mapM_ (uncurry $ outAction out) $ zip lst [0..]

outputCallback :: ChannelInfo c => SarasvatiConfig -> MVar [c] -> StreamCallback CFloat CFloat
outputCallback conf mvar _ _ frames _ out = do
  let frameLen = fromIntegral frames
  -- read list data
  list <- readMVar mvar
  (target, next) <- return $ splitAt frameLen list
  -- write to output pointer
  runOutput out target

  -- swap user data
  swapMVar mvar next 
  -- result value
  return $ if next == [] then Complete else Continue

----------------
-- hepler

data StreamState = Running | Finished deriving (Show, Eq, Read)

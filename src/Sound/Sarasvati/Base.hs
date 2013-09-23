module Sound.Sarasvati.Base
  ( SarasvatiConfig(..)
  , defaultConfig
  , sarasvatiOutput
  ) where
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Sound.Sarasvati.Types
import Sound.PortAudio 
import Foreign.C.Types (CFloat(..))
import Foreign.Ptr (Ptr)

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

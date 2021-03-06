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

data SarasvatiConfig = SarasvatiConfig 
  { confSampleRate :: SampleRate
  , confFramesPerBuffer :: Int
  }

defaultConfig :: SarasvatiConfig
defaultConfig = SarasvatiConfig 
  { confSampleRate = 44100
  , confFramesPerBuffer = 5000
  }

---------------
-- api

sarasvatiOutput 
  :: (Eq (w h), WavSeq w, Channel c h) => SarasvatiConfig -> w c -> IO (Either Error ())
sarasvatiOutput conf lst = 
  withPortAudio $ runSarasvatiOutput conf (fmap toChannelInfo lst)

runSarasvatiOutput 
  :: (Eq (w c), WavSeq w, ChannelInfo c) => SarasvatiConfig -> w c -> IO (Either Error ())
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

runOutput :: (WavSeq w, ChannelInfo c) => Ptr CFloat -> w c -> IO ()
runOutput out lst = runOutput' 0 lst
  where 
    runOutput' :: (WavSeq w, ChannelInfo c) => Int -> w c -> IO ()
    runOutput' i lst = case wavMatch lst of
      Nothing           -> return ()
      Just (x, Nothing) -> outAction out x i >> return ()
      Just (x, Just xs) -> outAction out x i >> runOutput' (i + 1) xs

outputCallback :: (Eq (w c), WavSeq w, ChannelInfo c) 
  => SarasvatiConfig -> MVar (w c) -> StreamCallback CFloat CFloat
outputCallback conf mvar _ _ frames _ out = do
  let frameLen = fromIntegral frames
  -- read list data
  list <- readMVar mvar
  (target, next) <- return $ wavSplitAt frameLen list
  -- write to output pointer
  runOutput out target

  -- swap user data
  swapMVar mvar next 
  -- result value
  return $ if next == wavNil then Complete else Continue

----------------
-- hepler

data StreamState = Running | Finished deriving (Show, Eq, Read)

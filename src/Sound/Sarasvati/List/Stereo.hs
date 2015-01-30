{-# LANGUAGE FlexibleInstances #-}
module Sound.Sarasvati.Base.List.Stereo where
import Control.Concurrent.MVar
import Foreign.C.Types (CFloat(..))
import Sound.PortAudio 
import Sound.Sarasvati.Types
import Foreign.Ptr (Ptr)

instance WavSeq [(Float, Float)] where
  sarasvatiOutput conf lst = withPortAudio $ runSarasvatiOutput conf (fmap float2Cfloat lst)

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

float2Cfloat :: (Float, Float) -> (CFloat, CFloat)
float2Cfloat = undefined

----------------
-- callback function

runOutput :: Ptr CFloat -> [(CFloat, CFloat)] -> IO ()
runOutput out lst = runOutput' 0 lst
  where 
    runOutput' :: Int -> [(CFloat, CFloat)] -> IO ()
    runOutput' i lst = undefined -- case wavMatch lst of
--      Nothing           -> return ()
--      Just (x, Nothing) -> outAction out x i >> return ()
--      Just (x, Just xs) -> outAction out x i >> runOutput' (i + 1) xs

outputCallback :: SarasvatiConfig -> MVar [(CFloat, CFloat)] -> StreamCallback CFloat CFloat
outputCallback conf mvar _ _ frames _ out = undefined
--  let frameLen = fromIntegral frames
--  -- read list data
--  list <- readMVar mvar
--  (target, next) <- return $ wavSplitAt frameLen list
--  -- write to output pointer
--  runOutput out target
--
--  -- swap user data
--  swapMVar mvar next 
--  -- result value
--  return $ if next == wavNil then Complete else Continue

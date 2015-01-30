{-# LANGUAGE FlexibleInstances #-}
module Sound.Sarasvati.List.Stereo () where
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.Types (CFloat(..))
import Sound.PortAudio 
import Sound.Sarasvati.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeElemOff)

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
float2Cfloat (x, y) = (CFloat x, CFloat y)

----------------
-- callback function

runOutput :: Ptr CFloat -> [(CFloat, CFloat)] -> IO ()
runOutput out lst = forM_ (zip [0..] lst) $ \(i, (v1, v2)) -> do
  pokeElemOff out (2 * i) v1
  pokeElemOff out (2 * i + 1) v2

outputCallback :: SarasvatiConfig -> MVar [(CFloat, CFloat)] -> StreamCallback CFloat CFloat
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

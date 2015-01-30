{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Sound.Sarasvati.Types where
import Sound.PortAudio 

type SampleRate = Double

data SarasvatiConfig = SarasvatiConfig 
  { confSampleRate :: SampleRate
  , confFramesPerBuffer :: Int
  }

defaultConfig :: SarasvatiConfig
defaultConfig = SarasvatiConfig 
  { confSampleRate = 44100
  , confFramesPerBuffer = 5000
  }

class WavSeq w where
  sarasvatiOutput :: SarasvatiConfig -> w -> IO (Either Error ())

data StreamState = Running | Finished deriving (Show, Eq, Read)

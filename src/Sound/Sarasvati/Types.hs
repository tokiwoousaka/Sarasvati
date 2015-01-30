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

-- ************************************************************* --
-- Old

--import Data.Foldable 
--import Control.DeepSeq
--import Foreign.C.Types (CFloat(..))
--import Foreign.Ptr (Ptr)
--import Foreign.Storable (Storable, pokeElemOff)
--
------------------
---- other names
--
--type Stereo = (Float, Float)
--type SampleRate = Double
--
------------------
---- sequence
--
--class (Foldable w, Functor w) => WavSeq w where
--  wavMatch :: w a -> Maybe (a, Maybe (w a))
--  wavNil :: w a
--  wavSplitAt :: Int -> w a -> (w a, w a)
--  wavFromList :: [a] -> w a
--  wavLength :: w a -> Int
--instance WavSeq [] where
--  wavMatch  []     = Nothing
--  wavMatch  (x:[]) = Just (x, Nothing)
--  wavMatch  (x:xs) = Just (x, Just xs)
--
--  wavNil = []
--  wavSplitAt = splitAt
--  wavFromList = id
--  wavLength = length
--
------------------
---- channels
--
--class (NFData c, Eq c , Eq h, ChannelInfo h) => Channel c h | c -> h where
--  toChannelInfo :: c -> h
--  makeWave :: [Float] -> [c]
--  emptyFrame :: c
--class Eq c => ChannelInfo c where
--  outAction :: Ptr CFloat -> c -> Int -> IO ()
--
---- stereo
--instance Channel (Float, Float) (CFloat, CFloat) where
--  toChannelInfo (v1,v2) = (CFloat v1, CFloat v2)
--  makeWave x = zip x x
--  emptyFrame = (0, 0)
--instance ChannelInfo (CFloat, CFloat) where
--  outAction out (v1, v2) i = do
--    pokeElemOff out (2 * i) v1
--    pokeElemOff out (2 * i + 1) v2
--
------------------
---- mixing
--
--class Mixable a where
--  mix  :: a -> a -> a
--
--(/+/) :: Mixable a => a -> a -> a
--(/+/) = mix
--
--instance Mixable a => Mixable [a] where
--  mix [] ys = ys
--  mix xs [] = xs
--  mix (x:xs) (y:ys) = mix x y : mix xs ys
--
---- stereo
--instance Mixable (Float, Float) where
--  mix (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

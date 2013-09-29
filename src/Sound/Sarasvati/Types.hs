{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Sound.Sarasvati.Types 
  ( Stereo
  , SampleRate
  , Channel(..)
  , ChannelInfo(..)
  , Mixable(..)
  , (/+/)
  ) where
import Foreign.C.Types (CFloat(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, pokeElemOff)

----------------
-- other names

type Stereo = (Float, Float)
type SampleRate = Double

----------------
-- channels

class (Eq c , Eq h, ChannelInfo h) => Channel c h | c -> h where
  toChannelInfo :: c -> h
  makeWave :: [Float] -> [c]
  emptyFrame :: c
class Eq c => ChannelInfo c where
  outAction :: Ptr CFloat -> c -> Int -> IO ()

-- stereo
instance Channel (Float, Float) (CFloat, CFloat) where
  toChannelInfo (v1,v2) = (CFloat v1, CFloat v2)
  makeWave x = zip x x
  emptyFrame = (0, 0)
instance ChannelInfo (CFloat, CFloat) where
  outAction out (v1, v2) i = do
    pokeElemOff out (2 * i) v1
    pokeElemOff out (2 * i + 1) v2

----------------
-- mixing

class Mixable a where
  mix  :: a -> a -> a

(/+/) :: Mixable a => a -> a -> a
(/+/) = mix

instance Mixable a => Mixable [a] where
  mix [] ys = ys
  mix xs [] = xs
  mix (x:xs) (y:ys) = mix x y : mix xs ys

-- stereo
instance Mixable (Float, Float) where
  mix (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

Sarasvati
=========

Sarasvati is an audio library with portaudio.

install
-----------------------

* install portaudio for you PC as http://portaudio.com/docs/v19-doxydocs/compile_linux.html

* install portaudio wrapper by cabal

```
$ cabal install portaudio
```

* install this library

```
$ cabal configure
$ cabal install
```

sample
-----------------------

```haskell
module Main where
import Sound.Sarasvati.Base

sinl :: [Float] 
sinl = take 100000 . map sin $ [0.0, 0.1..] 

suql :: [Float]
suql = take 100000 $ cycle (replicate 300 (-1) ++ replicate 300 1)

main :: IO ()
main = do
  sarasvatiOutput defaultConfig $ zip sinl sinl
  sarasvatiOutput defaultConfig $ zip suql suql
  return ()
```

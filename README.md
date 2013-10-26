Sarasvati
=========

Sarasvati is an audio library with portaudio.

Instration
-----------------------

* install portaudio for your PC

http://portaudio.com/docs/v19-doxydocs/compile_linux.html

* install this library from code

```
$ cabal configure
$ cabal install
```

* or from hackage

```
$ cabal install sarasvati
```

Sample
-----------------------

```haskell
module Main where
import Sound.Sarasvati

sinl :: [Float] 
sinl = take 100000 . map sin $ [0.0, 0.1..] 

squl :: [Float]
squl = take 100000 $ cycle (replicate 300 (-1) ++ replicate 300 1)

main :: IO ()
main = do
  sarasvatiOutput defaultConfig $ zip sinl sinl
  sarasvatiOutput defaultConfig $ zip squl squl
  return ()
```

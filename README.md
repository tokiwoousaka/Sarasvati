Sarasvati
=========

[![Join the chat at https://gitter.im/tokiwoousaka/Sarasvati](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/tokiwoousaka/Sarasvati?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Sarasvati is an audio library with portaudio.

Instration
-----------------------

### Install portaudio for your PC

if you use linux, see http://portaudio.com/docs/v19-doxydocs/compile_linux.html

### Install this library from code

```
$ cabal configure
$ cabal install
```

### Or from hackage

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

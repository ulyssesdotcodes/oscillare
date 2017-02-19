module Main where

import Oscillare

import Control.Concurrent

main :: IO ()
main = do
  (p, t, base, thread) <- run
  threadDelay 1000000
  base $ (+++) "a"
  threadDelay 1000000
  p $ pOver "a" "b c"
  threadDelay 1000000
  p $ pInput "c" BlankTex |+| pPaletteMap RibbonPalette ((+ 0.5) . sinMod' . (* 0.1)<$$> absTimePattern)
  threadDelay 1000000
  p $ pVoronoi "b" ((* 0.2) <$$> absTimePattern) |+| pPaletteMap RibbonPalette (sinMod' . (* 0.2) <$$> absTimePattern) |+| pRepeat (8 :: Double)
  threadDelay 1000000
  p $ pSine "b" ((* 1.2) <$$> absTimePattern) (1 :: Double) (1 :: Double) |+| pPaletteCycle EclecticPalette ((* 0.4) <$$> absTimePattern) |+| pFade (0.97 :: Double)
  threadDelay 1000000
  p $ pVoronoi "b" ((* 0.2) <$$> absTimePattern) |+| pPaletteMap RibbonPalette (sinMod' . (* 0.2) <$$> absTimePattern) |+| pRepeat (8 :: Double)
  threadDelay 30000000
  print "Done"

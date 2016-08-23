{-# LANGUAGE FlexibleInstances #-}
module Pattern where

import Data.Fixed

data Pattern a = Pattern { arc :: Double -> Double -> [a] }

instance Functor Pattern where
  fmap f (Pattern a) = Pattern (\p t -> f <$> a p t)

instance Applicative Pattern where
  pure as = Pattern $ const . const [as]
  Pattern fs <*> Pattern ms = Pattern (\p t -> fs p t <*> ms p t)

instance Monad Pattern where
  return = pure
  p >>= f = unwrap (f <$> p)

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap p = Pattern $ \pr t -> concatMap (\p' -> arc p' pr t) (arc p pr t)

instance Monoid (Pattern a) where
  mempty = Pattern $ const . const []
  mappend = overlay

overlay :: Pattern a -> Pattern a -> Pattern a
overlay p q = Pattern (\pr t -> arc p pr t ++ arc q pr t)

flatten :: Pattern [a] -> Pattern a
flatten ps = Pattern (\pr t -> concat (arc ps pr t))

once :: Pattern a -> Pattern a
once = att 0

att :: Double -> Pattern a -> Pattern a
att t p = Pattern (\pr cu -> if (match pr cu) then arc p pr cu else [])
  where
    matchSame pr cu = t >= pr && t < cu
    matchWrap pr cu = (cu < pr) && (t >= pr || t < cu)
    match pr cu = (matchSame pr cu || matchWrap pr cu)

offset :: Double -> Pattern a -> Pattern a
offset o (Pattern a) = Pattern (\pr t -> a ((pr - o) `mod'` 1) ((t - o) `mod'` 1))

seqp :: [Pattern a] -> Pattern a
seqp ps = seqpN 0 (length ps) ps

seqpN :: Int -> Int -> [Pattern a] -> Pattern a
seqpN i n (p:ps) = mappend (offset (seg * (fromIntegral i)) (Pattern timeGuard)) (seqpN (i+1) n ps)
  where
    seg = (1 :: Double) / (fromIntegral n)
    timeGuard pr t = if t >= 0 && t < seg then arc p pr t else []
seqpN _ _ [] = mempty

timePattern :: Pattern Double
timePattern = Pattern (\_ t -> [t])

deltaPattern :: Pattern Double
deltaPattern = Pattern (\p t -> [(t - p) `mod'` 1])

sinTimePattern :: Pattern Double
sinTimePattern = sin . (* 3.1415) <$> timePattern


class FloatPattern a where
  floatPattern :: a -> Pattern Double

instance FloatPattern Double where
  floatPattern = pure

instance FloatPattern (Pattern Double) where
  floatPattern = id

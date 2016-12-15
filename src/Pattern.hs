{-# LANGUAGE FlexibleInstances #-}
module Pattern where

import Prelude hiding (concat)

import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Fixed
import Data.List
import Data.Map.Strict (Map)

import System.Random

data PatternState = PatternState { pInputs :: Map ByteString Double
                                 , prevTime :: Double
                                 , curTime :: Double
                                 }

type Pattern a = Reader PatternState [a]

-- instance Pattern a => Functor Pattern where
--   fmap f m = f <$> runReaderT m

ffmap :: (a -> b) -> Pattern a -> Pattern b
ffmap f p = fmap f <$> p

infix 4 <$$>
(<$$>) :: (a -> b) -> Pattern a -> Pattern b
(<$$>) = ffmap

infix 4 <**>
(<**>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<**>) pf pa = unwrap $ ffmap (\f -> ffmap f pa) pf

ppure :: a -> Pattern a
ppure = pure . (:[])

-- instance Applicative Pattern where
--   pure as = Pattern $ const . const [as]
--   Pattern fs <*> Pattern ms = Pattern (\p t -> fs p t <*> ms p t)

-- instance Monad Pattern where
--   return = pure
--   p >>= f = unwrap (f <$> p)

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap a = do
  r <- ask
  return $ runReader (mconcat $ runReader a r) r

instance Monoid (Pattern a) where
  mempty = reader (const [])
  mappend a b = do
    ps <- ask
    return $ runReader a ps ++ runReader b ps

-- overlay :: Pattern a -> Pattern a -> Pattern a
-- overlay = Pattern (\pr t -> arc p pr t ++ arc q pr t)

-- flatten :: Pattern [a] -> Pattern a
-- flatten ps = Pattern (\pr t -> concat (arc ps pr t))

-- ffmap :: ([a] -> [b]) -> Pattern a -> Pattern b
-- ffmap f (Pattern as) = Pattern (\p t -> f $ as p t)

-- once :: Pattern a -> Pattern a
-- once = att 0

rev :: Double -> Double
rev x = 1 - x

flash :: Double -> Pattern a -> Pattern a -> Pattern a
flash t a b = do
  p <- ask
  let (PatternState _ pr cu) = p
  return $ if match pr cu then runReader a p else runReader b p
  where
    matchSame pr cu = t >= pr && t < cu
    matchWrap pr cu = (cu < pr) && (t >= pr || t < cu)
    match pr cu = (matchSame pr cu || matchWrap pr cu)

att :: Double -> Pattern a -> Pattern a
att t p = flash t p mempty

offset :: Double -> Pattern a -> Pattern a
offset d = local (\(PatternState i p c) -> PatternState i ((p - d) `mod'` 1) ((c - d) `mod'` 1))

seqp :: [Pattern a] -> Pattern a
seqp ps = seqpN 0 (length ps) ps

seqpN :: Int -> Int -> [Pattern a] -> Pattern a
seqpN i n (p:ps) = mappend (offset (seg * (fromIntegral i)) (reader timeGuard)) (seqpN (i+1) n ps)
  where
    seg = (1 :: Double) / (fromIntegral n)
    timeGuard (PatternState i' pr t) = if t >= 0 && t < seg then runReader p (PatternState i' pr t) else mempty
seqpN _ _ [] = mempty

timePattern :: Pattern Double
timePattern = (:[]) .  flip mod' 1 <$> reader curTime

absTimePattern :: Pattern Double
absTimePattern = (:[]) <$> reader curTime

-- abt = absTimePattern

deltaPattern :: Pattern Double
deltaPattern = reader (\(PatternState _ p c) -> [(c - p) `mod'` 1])

rand :: Pattern Double
rand = (fst . random . mkStdGen . round . (* (2 ^ 16))) <$$> timePattern

sinMod :: Double -> Double
sinMod = sin . (* (2 * pi))
sinMod' = (+ 0.5) . (* 0.5) . sinMod

cosMod :: Double -> Double
cosMod = cos . (* (2 * pi))
cosMod' = (+ 0.5) . (* 0.5) . cosMod

pi2Mod :: Double -> Double
pi2Mod = (* 6.2831)

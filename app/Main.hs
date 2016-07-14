{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Fixed
import Data.Text
import Data.Text.Encoding
import Data.Time.Clock
import Sound.OSC
import qualified Data.ByteString.Char8 as BS
import qualified Sound.OSC.Transport.FD as T

data Pattern a = Pattern { arc :: Int -> [a] } -- From 0-128

ostr = ASCII_String . encodeUtf8

main = do
  conn <- openUDP "127.0.0.1" 9001
  flip runReaderT conn $ sendOSC $ Message "/connection" []


instance Functor Pattern where
  fmap f (Pattern a) = Pattern (\t -> f <$> a t)

instance Applicative Pattern where
  pure p = Pattern $ const [p]
  Pattern fs <*> Pattern ms = Pattern (\t -> fs t <*> ms t)

perCycle :: Int -> Pattern a -> Pattern a
perCycle times (Pattern pf) = Pattern timeF
  where
    timeFrac = quot 128 times
    timeF t =
      if (t `mod` timeFrac) == 0 then pf t else []

timePattern :: Pattern Float
timePattern = Pattern $ (:[]) . (/ (128::Float)) . fromIntegral

data UniformType = UniformFloat Float

data Uniform a =  Uniform { name :: Text, value :: a }

timeUniform :: Pattern (Uniform Float)
timeUniform = uniformPattern "time" timePattern

uniformPattern :: Text -> Pattern Float -> Pattern (Uniform Float)
uniformPattern n = fmap (Uniform n)

data Program
  = Sine
  | Line
  | Scale

programText :: Program -> Text
programText Sine = "sine"
programText Line = "line"
programText Scale = "scale"

progName :: Text -> Program -> [Pattern (Uniform Float)] -> [Pattern Message]
progName n p us = progMsg:uMsgs
  where
    progMsg = perCycle 1 <$> pure $ Message "/progs" [ostr n, ostr $ programText p]
    uMsgs = (uniformMessage n <$>) <$> us

uniformMessage :: Text -> Uniform Float -> Message
uniformMessage n u = Message "/progs/uniform" [ostr n, ostr $ name u, float $ value u]

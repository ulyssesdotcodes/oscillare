{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Fixed
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time.Clock
import Sound.OSC
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified Sound.OSC.Transport.FD as T

data Pattern a = Pattern { arc :: Int -> [a] } -- From 0-128

ostr = ASCII_String . encodeUtf8

main = do
  now <- getCurrentTime
  conn <- openUDP "127.0.0.1" 3333
  mvarT <- newMVar $ TempoState conn (progName (pack "p1") Sine [timeUniform, perCycle 2 $ uniformPattern (pack "scale") (sin <$> (* 3.1415) <$> timePattern)]) (utctDayTime now) (secondsToDiffTime 1) 0
  sync mvarT

instance Functor Pattern where
  fmap f (Pattern a) = Pattern (\t -> f <$> a t)

instance Applicative Pattern where
  pure p = Pattern $ const [p]
  Pattern fs <*> Pattern ms = Pattern (\t -> fs t <*> ms t)

instance Monad Pattern where
  return = pure
  p >>= f = unwrap (f <$> p)

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap p = Pattern $ \t -> concatMap (`arc` t) (arc p t)

instance Monoid (Pattern a) where
  mempty = Pattern $ const []
  mappend = overlay

overlay :: Pattern a -> Pattern a -> Pattern a
overlay p q = Pattern (\t -> arc p t ++ arc q t)

flatten :: Pattern [a] -> Pattern a
flatten ps = Pattern (\t -> concat (arc ps t))

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

progName :: Text -> Program -> [Pattern (Uniform Float)] -> Pattern Message
progName n p us = mappend progMsg $ mconcat uMsgs
  where
    progMsg = perCycle 1 <$> pure $ Message "/progs" [ostr n, ostr $ programText p]
    uMsgs = (uniformMessage n <$>) <$> us

uniformMessage :: Text -> Uniform Float -> Message
uniformMessage n u = Message "/progs/uniform" [ostr n, ostr $ name u, float $ value u]

data TempoState = TempoState { conn :: UDP, pattern :: Pattern Message, start :: DiffTime, cycleLength :: DiffTime, current :: Int }

updateCycle :: MonadIO io => DiffTime -> StateT TempoState io ()
updateCycle now = do
  tState <- get
  let diffTime = now - start tState
  if diffTime > cycleLength tState then
    put $ tState { start = now, current = 0 }
  else
    put $ tState { current = round (diffTime * (fromIntegral 128)) }

sendMessages :: MonadIO io => StateT TempoState io ()
sendMessages = do
  tState <- get
  -- liftIO $ T.sendOSC (conn tState) $ Bundle 0 $ arc (pattern tState) (current tState)
  liftIO $ print $ arc (pattern tState) (current tState)

frame :: MonadIO io => StateT TempoState io ()
frame = do
  now <- liftIO getCurrentTime
  sendMessages
  updateCycle $ utctDayTime now

sync :: MVar TempoState -> IO ()
sync ts = do
  now <- utctDayTime <$> getCurrentTime
  modifyMVar_ ts (\ts' -> snd <$> runStateT frame ts')
  now' <- utctDayTime <$> getCurrentTime
  threadDelay (16000 - round ((now' - now) * 1000))
  sync ts

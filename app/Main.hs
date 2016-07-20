{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Fixed
import Data.Map.Strict
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time.Clock
import Sound.OSC
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified Sound.OSC.Transport.FD as T

data Pattern a = Pattern { arc :: Float -> [a] }

data TempoState = TempoState { _conn :: UDP, _pattern :: Map Text (Pattern Message), _start :: DiffTime, _cycleLength :: DiffTime, _current :: Float }

makeLenses ''TempoState

instance Show TempoState where
  show (TempoState c p s cy cu) = "{ messages " ++ show (arc (addName `foldMapWithKey` p) cu) ++ " cu " ++ (show cu) ++ " }"

ostr = ASCII_String . encodeUtf8

main = do
  mvarT <- revEngines
  sync mvarT

revEngines :: IO (MVar TempoState)
revEngines = do
  now <- getCurrentTime
  conn <- openUDP "127.0.0.1" 9001
  mVarT <- newMVar $ TempoState conn (fromList []) (utctDayTime now) (secondsToDiffTime 1) 0
  return mVarT

gunEngines :: MVar TempoState -> IO (ThreadId)
gunEngines = forkIO . sync

run :: IO ((String -> Pattern Message -> IO ()), (Float -> IO ()), ThreadId)
run = do
  mts <- revEngines
  ti <- gunEngines mts
  return (runProg mts, changeTempo mts, ti)

setProg :: Text -> Pattern Message -> TempoState -> TempoState
setProg n p = over pattern (insert n p)

runProg :: MVar TempoState -> String -> Pattern Message -> IO ()
runProg mts t p = modifyMVar_ mts $ return <$> setProg (pack t) p

changeTempo :: MVar TempoState -> Float -> IO ()
changeTempo mts t = modifyMVar_ mts $ return <$> set cycleLength (fromRational $ toRational t)

sineProg :: Pattern Message
sineProg = programMessage (progName Sine) (Just "p1") [timeUniform]

scaleEffect :: Pattern Message
scaleEffect = programMessage (effectName Scale) Nothing [uniformPattern "scale" (uf . (* 0.5) . sin . (* (3.1415 * 2)) <$> timePattern)]

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
      if (round (t * 128) `mod` timeFrac) == 0 then pf t else []

timePattern :: Pattern Float
timePattern = Pattern $ (:[])

data UniformType = UniformFloat Float | UniformInput (InputType, Float)

data InputType = AudioTexture | Volume

inputText :: InputType -> Text
inputText AudioTexture = "audio_texture"
inputText Volume = "volume"

uf = UniformFloat
ui = UniformInput


data Uniform a =  Uniform { name :: Text, value :: a }

timeUniform :: Pattern (Uniform UniformType)
timeUniform = uniformPattern "time" (UniformFloat <$> timePattern)

uniformPattern :: Text -> Pattern UniformType -> Pattern (Uniform UniformType)
uniformPattern n = fmap (Uniform n)

up :: String -> Pattern UniformType -> Pattern (Uniform UniformType)
up s = uniformPattern (pack s)

data Program
  = Sine
  | Line
  | AudioData

data Effect
  = Scale
  | Fade

programText :: Program -> Text
programText Sine = "sine"
programText Line = "line_down"
programText AudioData = "audio_data"

effectText :: Effect -> Text
effectText Scale = "scale"
effectText Fade = "fade"

data ProgramName a = ProgramName { prog :: a, nameF :: (a -> Text) }

progName :: Program -> ProgramName Program
progName t = ProgramName t programText

effectName :: Effect -> ProgramName Effect
effectName t = ProgramName t effectText

programMessage :: ProgramName a -> Maybe Text -> [Pattern (Uniform UniformType)] -> Pattern Message
programMessage p me us = mconcat $ (progMsg:maybe [clearMsg] ((:[]) . effectMsg) me) ++ uMsgs
  where
    effectMsg e = perCycle 1 <$> pure $ Message "/progs/effect" [ostr e]
    clearMsg = perCycle 1 <$> pure $ Message "/progs/effect/clear" []
    progMsg = perCycle 1 <$> pure $ Message "/progs" [ostr $ nameF p (prog p)]
    uMsgs = (uniformMessage <$>) <$> us

pme :: ProgramName a -> String -> [Pattern (Uniform UniformType)] -> Pattern Message
pme p t = programMessage p (Just $ pack t)

pm :: ProgramName a -> [Pattern (Uniform UniformType)] -> Pattern Message
pm p = programMessage p Nothing

uniformMessage :: Uniform UniformType -> Message
uniformMessage (Uniform n (UniformFloat f)) =
  Message "/progs/uniform" [ostr n, float f]
uniformMessage (Uniform n (UniformInput (i, m))) =
  Message "/progs/uniform" [ostr n, ostr $ inputText i, float m]

zipui :: InputType -> Float -> UniformType
zipui i f = UniformInput (i, f)

addName :: Text -> Pattern Message -> Pattern Message
addName n p = appendDatum (ostr n) <$> p

appendDatum :: Datum -> Message -> Message
appendDatum d (Message a ds) = Message a (d:ds)

updateCycle :: MonadIO io => DiffTime -> StateT TempoState io ()
updateCycle now = do
  tState <- get
  let diffTime = now - view start tState
  if diffTime > view cycleLength tState then
    put $ tState & start .~ now & current .~ 0
  else
    put $ tState & current .~ (fromRational . toRational) (diffTime / view cycleLength tState)

sendMessages :: MonadIO io => StateT TempoState io ()
sendMessages = do
  tState <- get
  liftIO $ T.sendOSC (view conn tState) $ Bundle 0 $ arc (addName `foldMapWithKey` (view pattern tState)) (view current tState)
  -- liftIO $ print $ arc (addName `foldMapWithKey` pattern tState) (current tState)

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

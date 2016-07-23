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

data Pattern a = Pattern { arc :: Float -> Float -> [a] }

data TempoState = TempoState { _conn :: UDP, _pattern :: Map Text (Pattern Message), _start :: DiffTime, _cycleLength :: DiffTime, _prev :: Float, _current :: Float }

makeLenses ''TempoState

instance Show TempoState where
  show (TempoState c p s cy pr cu) = "{ messages " ++ show (arc (addName `foldMapWithKey` p) pr cu) ++ " cu " ++ (show cu) ++ " }"

ostr = ASCII_String . encodeUtf8

main = do
  mvarT <- revEngines
  sync mvarT

revEngines :: IO (MVar TempoState)
revEngines = do
  now <- getCurrentTime
  conn <- openUDP "127.0.0.1" 9001
  mVarT <- newMVar $ TempoState conn (fromList []) (utctDayTime now) (secondsToDiffTime 1) 0 0
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

att :: Float -> Pattern a -> Pattern a
att t' p = Pattern (\pr t -> if t' >= pr && t' < t then arc p pr t else [])

timePattern :: Pattern Float
timePattern = Pattern (\_ t -> [t])

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
    effectMsg e = att 0 <$> pure $ Message "/progs/effect" [ostr e]
    clearMsg = att 0 <$> pure $ Message "/progs/effect/clear" []
    progMsg = att 0 <$> pure $ Message "/progs" [ostr $ nameF p (prog p)]
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
    put $ tState & current .~ (fromRational . toRational) (diffTime / view cycleLength tState) & prev .~ view current tState

sendMessages :: MonadIO io => StateT TempoState io ()
sendMessages = do
  tState <- get
  liftIO $ T.sendOSC (view conn tState) $ Bundle 0 $ arc (addName `foldMapWithKey` (view pattern tState)) (view prev tState) (view current tState)
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

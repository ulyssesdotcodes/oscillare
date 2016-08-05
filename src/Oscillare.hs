{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Oscillare where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Fixed
import Data.Map.Strict
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time.Clock
import Network.Socket
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T

data Pattern a = Pattern { arc :: Float -> Float -> [a] }

data TempoState = TempoState { _conn :: UDP, _pattern :: Map Text (Pattern Message), _start :: DiffTime, _cycleLength :: DiffTime, _prev :: Float, _current :: Float }

makeLenses ''TempoState

instance Show TempoState where
   show (TempoState _ p _ _ pr cu) = "{ messages " ++ show (arc (addName `foldMapWithKey` p) pr cu) ++ " pr " ++ (show pr) ++ " cu " ++ (show cu) ++ " }"

ostr = ASCII_String . encodeUtf8

revEngines :: IO (MVar TempoState)
revEngines = do
  now <- getCurrentTime
  conn' <- openUDP "127.0.0.1" 9001
  mVarT <- newMVar $ TempoState conn' (fromList []) (utctDayTime now) (secondsToDiffTime 1) 0 0
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

att :: Float -> Pattern a -> Pattern a
att t p = Pattern (\pr cu -> if (match pr cu) then arc p pr cu else [])
  where
    matchSame pr cu = t >= pr && t < cu
    matchWrap pr cu = (cu < pr) && (t >= pr || t < cu)
    match pr cu = (matchSame pr cu || matchWrap pr cu)

offset :: Float -> Pattern a -> Pattern a
offset o (Pattern a) = Pattern (\pr t -> a ((pr - o) `mod'` 1) ((t - o) `mod'` 1))

seqp :: [Pattern a] -> Pattern a
seqp ps = seqpN 0 (length ps) ps

seqpN :: Int -> Int -> [Pattern a] -> Pattern a
seqpN i n (p:ps) = mappend (offset (seg * (fromIntegral i)) (Pattern timeGuard)) (seqpN (i+1) n ps)
  where
    seg = (1 :: Float) / (fromIntegral n)
    timeGuard pr t = if t >= 0 && t < seg then arc p pr t else []
seqpN _ _ [] = mempty

timePattern :: Pattern Float
timePattern = Pattern (\_ t -> [t])

deltaPattern :: Pattern Float
deltaPattern = Pattern (\p t -> [(t - p) `mod'` 1])

sinTimePattern :: Pattern Float
sinTimePattern = sin . (* 3.1415) <$> timePattern

data UniformType = UniformFloat Float | UniformInput (InputType, Float)

data InputType = AudioTexture | Volume | EqTexture

inputText :: InputType -> Text
inputText AudioTexture = "audio_texture"
inputText Volume = "volume"
inputText EqTexture = "eq_texture"

uf = UniformFloat
ui = UniformInput


data Uniform a =  Uniform { name :: Text, value :: a }

timeUniform :: Pattern (Uniform UniformType)
timeUniform = uniformPattern "time" (UniformFloat <$> timePattern)

deltaUniform :: Pattern (Uniform UniformType)
deltaUniform = uniformPattern "delta" $ UniformFloat <$> deltaPattern

uniformPattern :: Text -> Pattern UniformType -> Pattern (Uniform UniformType)
uniformPattern n = fmap (Uniform n)

up :: String -> Pattern UniformType -> Pattern (Uniform UniformType)
up = uniformPattern . pack

data Program =
  Add
  | AudioData
  | Brightness
  | Dots
  | Fade
  | Filter
  | Flocking
  | Line
  | Mult
  | Repeat
  | Scale
  | Sine
  | StringTheory

programText :: Program -> Text
programText Add = "add"
programText AudioData = "audio_data"
programText Brightness = "brightness"
programText Dots = "dots"
programText Fade = "fade"
programText Filter = "filter"
programText Flocking = "flocking"
programText Line = "line_down"
programText Mult = "mult"
programText Scale = "scale"
programText Sine = "sine"
programText StringTheory = "string_theory"
programText Repeat = "repeat"

noUProgram :: Program -> String -> Pattern Message
noUProgram p next = pme p next [] []

singleUProgram :: Program -> String -> Pattern Float -> Pattern Message
singleUProgram p next uVal = pmeT p (pack next) [programText p] [uf <$> uVal]


pAdd = noUProgram Add
pAudioData next uVolume uInput uVal = pme AudioData next ["tex_audio", "volume"] [(zipui uInput) <$> uVal, uf <$> uVolume]
pBrightness = singleUProgram Brightness
pDots next uVolume uInput uVal = pme Dots next ["eqs", "invVolume"] [(zipui uInput) <$> uVal, uf <$> uVolume]
pFade = singleUProgram Fade
pFilter = singleUProgram Filter
-- Have this on the other branch: pFlocking next = pme Flocking 
pMult = noUProgram Mult
pScale = singleUProgram Scale
pSine next uTimeMod uScale uAmplitude = pme Sine next ["time", "scale", "amplitude"] [uf . uTimeMod <$> timePattern, uf <$> uScale, uf <$> uAmplitude]
pStringTheory next uAngle uAngleDelta uXoff = pme StringTheory next ["angle", "angle_delta", "xoff"] [uAngle, uAngleDelta, uXoff]
pRepeat = singleUProgram Repeat

programMessage :: Program -> Maybe (Text) -> [Pattern (Uniform UniformType)] -> Pattern Message
programMessage p me us = mconcat $ (progMsg:maybe [clearMsg] ((:[]) . effectMsg) me) ++ uMsgs
  where
    effectMsg e = once <$> pure $ Message "/progs/effect" [ostr e]
    clearMsg = once <$> pure $ Message "/progs/effect/clear" []
    progMsg = once <$> pure $ Message "/progs" [ostr $ programText p]
    uMsgs = (uniformMessage <$>) <$> us

passthrough :: Pattern Text -> Pattern Message
passthrough ep = mappend progMsg effectMsg
   where
     effectMsg = (\e -> Message "/progs/effect" [ostr e]) <$> ep
     progMsg = once <$> pure $ Message "/progs" [ostr $ pack "passthrough"]

pt :: Pattern String -> Pattern Message
pt = passthrough . fmap pack


pme :: Program -> String -> [String] -> [Pattern UniformType] -> Pattern Message
pme p t uns us = programMessage p (Just $ pack t) (zipWith up uns us)

pmeT :: Program -> Text -> [Text] -> [Pattern UniformType] -> Pattern Message
pmeT p t uns us = programMessage p (Just t) (zipWith uniformPattern uns us)

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

oscConnected :: UDP -> IO Bool
oscConnected (UDP sock) = isBound sock

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
  -- liftIO $ print tState
  liftIO $ T.sendOSC (conn' tState) $ Bundle 0 $ arc (addName `foldMapWithKey` (view pattern tState)) (view prev tState) (view current tState)
  -- liftIO $ print $ arc (addName `foldMapWithKey` pattern tState) (current tState)
  where
    conn' ts = view conn ts

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Runner where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Char
import Data.Map.Strict (Map, insert)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import Data.Time.Clock
import Network.Socket
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T

import Pattern
import Program
import Uniform

data TempoState = TempoState { _conn :: UDP, _pattern :: Map Text Program, _start :: DiffTime, _cycleLength :: DiffTime, _prev :: Double, _current :: Double }

makeLenses ''TempoState

-- instance Show TempoState where
--    show (TempoState _ p _ _ pr cu) = "{ messages " ++ show (arc (addName `foldMapWithKey` p) pr cu) ++ " pr " ++ (show pr) ++ " cu " ++ (show cu) ++ " }"

ostr = ASCII_String . encodeUtf8

revEngines :: IO (MVar TempoState)
revEngines = do
  now <- getCurrentTime
  conn' <- openUDP "127.0.0.1" 9001
  mVarT <- newMVar $ TempoState conn' mempty (utctDayTime now) (secondsToDiffTime 1) 0 0
  return mVarT

gunEngines :: MVar TempoState -> IO (ThreadId)
gunEngines = forkIO . sync

run :: IO ((Program -> IO ()), (Double -> IO ()), ThreadId)
run = do
  mts <- revEngines
  ti <- gunEngines mts
  return (runProg mts, changeTempo mts, ti)

setProg :: Program -> TempoState -> TempoState
setProg p = over pattern (insert (programSlot p) p)

runProg :: MVar TempoState -> Program -> IO ()
runProg mts p = modifyMVar_ mts $ return <$> setProg p

changeTempo :: MVar TempoState -> Double -> IO ()
changeTempo mts t = modifyMVar_ mts $ return <$> set cycleLength (fromRational $ toRational t)

data SlotMessages = SlotMessages { slot :: Slot, messages :: Pattern Message }

programMessage :: Program -> Pattern Message
programMessage (Program slot (SlottableProgram (BaseProgram prog us effs))) =
  slotMessages slot (BaseName prog) (nextSlot baseSlot) us `mappend`
    (effectsMessages baseSlot $ reverse effs)
  where
    baseSlot = pack $ unpack slot ++ "0"
programMessage (Program slot (Passthrough sp)) = addSlot slot $ passthrough sp
programMessage (Program slot (Layer l ss effs)) =
    (addSlot slot . once . mconcat $ pure <$> [progMsg, effMsg, layerMsg])
      `mappend` (effectsMessages baseSlot $ reverse effs)
  where
    baseSlot = pack $ unpack slot ++ "0"
    progMsg = Message "/progs" [ostr (progName $ LayerName l)]
    layerMsg = Message "/progs/connections" $ ostr <$> ss
    effMsg = Message "/progs/effect" [ostr $ nextSlot baseSlot]

effectsMessages :: Slot -> [Effect] -> Pattern Message
effectsMessages s ((Effect e us):es) =
  slotMessages s' (EffName e) s'' us `mappend` effectsMessages s' es
  where
    s' = nextSlot s
    s'' = nextSlot s'
effectsMessages s [] = once <$> pure $ Message "/progs/effect/clear" [ostr s]

nextSlot :: Slot -> Slot
nextSlot s = pack $ head s':show (num + 1)
  where
    s' = unpack s
    num = read $ tail s'

slotMessages :: Slot -> Name -> Slot -> Pattern Uniform -> Pattern Message
slotMessages s n next us = addSlot s $ mconcat $ [progMsg, effectMsg, uMsgs us]
  where
    effectMsg = once <$> pure $ Message "/progs/effect" [ostr next]
    progMsg = once <$> pure $ Message "/progs" [ostr (progName n)]
    uMsgs us' = uniformMessage <$> us'

passthrough :: Pattern Text -> Pattern Message
passthrough ep = mappend progMsg effectMsg
   where
     effectMsg = (\e -> Message "/progs/effect" [ostr e]) <$> ep
     progMsg = once <$> pure $ Message "/progs" [ostr $ pack "passthrough"]

pt :: String -> Pattern String -> Program
pt s sp = Program (pack s) (Passthrough $ pack <$> sp)

uniformMessage :: Uniform -> Message
uniformMessage (Uniform n (UniformFloat f)) =
  Message "/progs/uniform" [ostr n, float f]
uniformMessage (Uniform n (UniformInput (i, m))) =
  Message "/progs/uniform" [ostr n, ostr $ inputText i, float m]

addSlot :: Text -> Pattern Message -> Pattern Message
addSlot n p = appendDatum (ostr n) <$> p

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
  liftIO $ T.sendOSC (conn' tState) $ Bundle 0 $ arc (foldr (mappend . programMessage) mempty (view pattern tState)) (view prev tState) (view current tState)
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

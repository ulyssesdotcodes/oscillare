{-# LANGUAGE OverloadedStrings #-}

module Runner where

import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Map.Strict (Map, insert, foldMapWithKey)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Time.Clock
import Network.Socket
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T

import Pattern
import Program
import Server
import State
import Uniform

ostr = ASCII_String

instance Show TempoState where
   show (TempoState _ p _ _ pr cu) =
     let
       addName n p = (unpack n) ++ ": " ++ show (arc (programMessage p) pr cu)
       msgs = addName `foldMapWithKey` p
     in
      case msgs of
        [] -> ""
        e -> "{ messages " ++ e ++ " pr " ++ (show pr) ++ " cu " ++ (show cu) ++ " }"

revEngines :: IO (MVar TempoState)
revEngines = do
  now <- getCurrentTime
  conn' <- openUDP "127.0.0.1" 9001
  newMVar $ TempoState conn' mempty (utctDayTime now) (secondsToDiffTime 1) 0 0

gunEngines :: MVar TempoState -> IO ThreadId
gunEngines = forkIO . sync

run :: IO ((Program -> IO ()), (Double -> IO ()), ThreadId)
run = do
  mts <- revEngines
  ti <- gunEngines mts
  _ <- forkIO $ do
    udpsrv <- udpServer "0.0.0.0" 9000
    runReaderT (serve mts) udpsrv
  return (runProg mts, changeTempo mts, ti)

runProg :: MVar TempoState -> Program -> IO ()
runProg mts p = modifyMVar_ mts $ \ts -> do
  (pLog, ts') <- runStateT (setProg' p) ts
  putStrLn pLog
  return ts'

changeTempo :: MVar TempoState -> Double -> IO ()
changeTempo mts t = modifyMVar_ mts $ return <$> set cycleLength (fromRational $ toRational t)

data SlotMessages = SlotMessages { slot :: Slot, messages :: Pattern Message }

baseSlot slot = pack $ unpack slot ++ "0"

programMessage :: Program -> Pattern Message
programMessage (Program slot (SlottableProgram (BaseProgram prog us effs))) =
  slotMessages (baseSlot slot) (BaseName prog) (nextSlot (baseSlot slot)) us `mappend`
    (effectsMessages (baseSlot slot) $ reverse effs)
programMessage (Program slot (Layer l ss es)) =
  case es of
    [] -> (addSlot (baseSlot slot) . once . mconcat $ pure <$> [progMsg l, layerMsg ss])
    e:effs ->
      (addSlot (baseSlot slot) . once . mconcat $ pure <$> [progMsg l, effMsg, layerMsg ss])
      `mappend` (effectsMessages (baseSlot slot) $ reverse (e:effs))
  where
    progMsg l = Message "/progs" [ostr (progName $ LayerName l)]
    layerMsg ss = Message "/progs/connections" $ ostr . baseSlot <$> ss
    effMsg = Message "/progs/effect" [ostr $ nextSlot (baseSlot slot)]

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

uniformMessage :: Uniform -> Message
uniformMessage (Uniform n (UniformFloatValue (FloatDoubleValue f))) =
  Message "/progs/uniform" [ostr n, float f]
uniformMessage (Uniform n (UniformFloatValue (FloatInputValue f m))) =
  Message "/progs/uniform" $ [ostr n, ostr $ floatInputText f] ++ (float <$> m)
uniformMessage (Uniform n (UniformTexValue (TexInputValue i m))) =
  Message "/progs/uniform" [ostr n, ostr $ texInputText i, float m]
uniformMessage (Uniform n (UniformStringValue s)) =
  Message "/progs/uniform" [ostr n, ostr s]

addSlot :: ByteString -> Pattern Message -> Pattern Message
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
  -- liftIO $ if (show tState == "") then return () else print tState
  liftIO $ T.sendOSC (conn' tState) $ Bundle 0 $ arc (foldr (mappend . programMessage) mempty (view patt tState)) (view prev tState) (view current tState)
  where
    conn' = view conn

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

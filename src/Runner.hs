{-# LANGUAGE OverloadedStrings #-}

module Runner where

import Prelude hiding (unwords, lookup)

import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Map.Strict (Map, insert, foldMapWithKey, member, (!), lookup)
import Data.ByteString.Char8 (ByteString, pack, unpack, unwords, append)
import Data.Fixed
import Data.List (nub, (\\))
import Data.Map.Strict (Map, insert, foldMapWithKey)
import Data.Maybe
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
   show (TempoState _ p _ _ pr cu exec inputs _) =
     let
       addName n p = (unpack n) ++ ": " ++ show (arc (programMessage inputs p) pr cu)
       msgs = addName `foldMapWithKey` p
     in
      case msgs of
        [] -> ""
        e -> "{ messages " ++ e ++ " pr " ++ (show pr) ++ " cu " ++ (show cu) ++ " exec " ++ (show exec) ++ " }"

revEngines :: IO (MVar TempoState)
revEngines = do
  now <- getCurrentTime
  conn' <- openUDP "127.0.0.1" 9001
  newMVar $ TempoState conn' mempty (utctDayTime now) (secondsToDiffTime 1) 0 0 (Exec [] (fPattern (8 :: Double)) []) mempty []

gunEngines :: MVar TempoState -> IO ThreadId
gunEngines = forkIO . sync

run :: IO ((Program -> IO ()), (Double -> IO ()), (TempoState -> TempoState) -> IO (), ThreadId)
run = do
  mts <- revEngines
  ti <- gunEngines mts
  _ <- forkIO $ do
    udpsrv <- udpServer "0.0.0.0" 9000
    runReaderT (serve mts) udpsrv
  return (runProg mts, \t -> modifyMVar_ mts (runReaderT (changeTempo t)), \f -> modifyMVar_ mts (return . f), ti)

runProg :: MVar TempoState -> Program -> IO ()
runProg mts p = modifyMVar_ mts $ \ts -> do
  (pLog, ts') <- runStateT (setProg' p) ts
  putStrLn pLog
  return $ resetCache ts'

data SlotMessages = SlotMessages { dest :: Slot, messages :: Pattern Message }

baseSlot :: ByteString -> ByteString
baseSlot s = append s (pack "0")

execMessage :: Map ByteString Double -> Exec -> Pattern Message
execMessage inputs (Exec ps k es) =
  programMessage inputs (Program (pack "s") (SlottableProgram (BaseProgram TriggeredPassthrough us es)))
  where
    us = uniformPattern "program" (pure . UniformStringValue $ unwords ps) `mappend` (uniformPattern "trigger" $ UniformFloatValue <$> k)

programMessage :: Map ByteString Double -> Program -> Pattern Message
programMessage inputs (Program s (SlottableProgram (BaseProgram prog us effs))) =
  slotMessages (baseSlot s) (BaseName prog) (nextSlot (baseSlot s)) us inputs `mappend`
    (effectsMessages inputs (baseSlot s) $ reverse effs)
programMessage inputs (Program slot (Layer l ss es)) =
  case es of
    [] -> (addSlot (baseSlot slot) . mconcat $ pure <$> [progMsg l, layerMsg ss])
    e:effs ->
      (addSlot (baseSlot slot) . mconcat $ pure <$> [progMsg l, effMsg, layerMsg ss])
      `mappend` (effectsMessages inputs (baseSlot slot) $ reverse (e:effs))
  where
    progMsg l = Message "/progs" [ostr (progName $ LayerName l)]
    layerMsg ss = Message "/progs/connections" $ ostr . baseSlot <$> ss
    effMsg = Message "/progs/effect" [ostr $ nextSlot (baseSlot slot)]
programMessage _ (Program slot Blank) = pure $ Message "/progs/clear" [ostr slot]

effectsMessages :: Map ByteString Double -> Slot -> [Effect] -> Pattern Message
effectsMessages inputs s ((Effect e us):es) =
  slotMessages s' (EffName e) s'' us inputs `mappend` effectsMessages inputs s' es
  where
    s' = nextSlot s
    s'' = nextSlot s'
effectsMessages _ s [] = pure $ Message "/progs/effect/clear" [ostr s]

nextSlot :: Slot -> Slot
nextSlot s = pack $ fromMaybe ("no: " ++ (unpack s)) $ init' s' >>= \i -> num >>= \n -> Just (i ++ show (n + 1))
  where
    s' = unpack s
    num = tail' s' >>= readMaybe
    tail' [] = Nothing
    tail' xs = Just [(last xs)]
    init' [] = Nothing
    init' xs = Just (init xs)

slotMessages :: Slot -> Name -> Slot -> Pattern Uniform -> Map ByteString Double -> Pattern Message
slotMessages s n next us inputs = addSlot s $ mconcat $ [progMsg, effectMsg, uMsgs us]
  where
    effectMsg = pure $ Message "/progs/effect" [ostr next]
    progMsg = pure $ Message "/progs" [ostr (progName n)]
    uMsgs us' = uniformMessage inputs <$> us'

uniformMessage :: Map ByteString Double -> Uniform -> Message
uniformMessage _ (Uniform n (UniformFloatValue (FloatDoubleValue f))) =
  Message "/progs/uniform" [ostr n, float f]
uniformMessage inputMap (Uniform n (UniformFloatValue (FloatInputValue f m))) =
  case member (floatInputText f) inputMap of
    True -> Message "/progs/uniform" $ [ostr n, (float . (* (head m))) (fromMaybe 0 (lookup (floatInputText f) inputMap))] ++ (float <$> m)
    False -> Message "/progs/uniform" $ [ostr n, ostr "input", ostr $ floatInputText f] ++ (float <$> m)
uniformMessage _ (Uniform n (UniformTexValue (TexInputValue i m))) =
  Message "/progs/uniform" [ostr n, ostr "input", ostr $ texInputText i, float (head m)]
uniformMessage _ (Uniform n (UniformStringValue s)) =
  Message "/progs/uniform" [ostr n, ostr "string", ostr s]

addSlot :: ByteString -> Pattern Message -> Pattern Message
addSlot n p = appendDatum (ostr n) <$> p

appendDatum :: Datum -> Message -> Message
appendDatum d (Message a ds) = Message a (d:ds)

oscConnected :: UDP -> IO Bool
oscConnected (UDP sock) = isBound sock

updateCycle :: MonadIO io => DiffTime -> StateT TempoState io ()
updateCycle now = do
  tState <- get
  prev .= view current tState
  current .= ((fromRational . toRational) $ now / (view cycleLength tState))

sendMessages :: MonadIO io => StateT TempoState io ()
sendMessages = do
  tState <- get
  -- liftIO $ if (show tState == "") then return () else print tState
  let messages = arc (execMessage (view inputs tState) (view exec tState) `mappend` foldr (mappend . programMessage (view inputs tState)) mempty (view patt tState)) (view prev tState) (view current tState)
  -- liftIO $ putStrLn (show b)
  liftIO $ T.sendOSC (conn' tState) $ Bundle 0 $ (flip (\\) $ view lastMessages tState) . nub $ messages
  lastMessages .= messages
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

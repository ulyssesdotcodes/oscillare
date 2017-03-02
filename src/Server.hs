{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Server where

import Prelude hiding (concat, lookup)

import Control.Concurrent
import Control.Lens
import Control.Lens.TH
import Control.Lens.Zoom
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Aeson (decode)
import Data.ByteString.Char8 (ByteString, concat, pack, readInt, split, unpack, append)
import Data.Char
import Data.List (unionBy, (\\), deleteBy)
import Data.Map.Lazy (Map, fromList, (!), lookup, insert)
import Data.Maybe
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Float
import Network.Wai
import Network.Wai.Application.Static (StaticSettings(..), staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import qualified Sound.OSC as OSC
import Sound.OSC.Transport.FD as T


import Osc
import Pattern
import Program
import State
import Uniform

type MessageQueue = Chan OSC.Message


data ServerState = ServerState { _ts :: MVar TempoState, _toggles :: ActiveToggles }
type ActiveToggles = [(Int,Int)]

makeLenses ''ServerState

launchControlXLMap :: Map Int ByteString
launchControlXLMap = fromList $ (map (\n -> (n, pack $ "pot" ++ (show . (+ 1) $ (n - 13) `mod` 8) ++ [chr $ (quot (n - 13) 8) + 97])) [13..37]) ++
  ((map (\n -> (n, pack $ "fader" ++ show (n - 76))) [77..85])) ++
  ((map (\n -> (n, pack $ "button" ++ show (n - 40))) [41..57]))

serverState :: MVar TempoState -> IO (MVar ServerState)
serverState mts = newMVar $ ServerState mts []

serve :: (T.Transport t) => MVar TempoState -> ReaderT t IO ()
serve mts = do
  ss <- liftIO $ serverState mts
  _ <- liftIO . forkIO $ run 8000 $ websocketsOr defaultConnectionOptions (wsApp ss) staticPage
  udpRecvMsg ss

staticPage :: Application
staticPage =
  staticApp $ defaultWebAppSettings "C:\\Users\\ulyss\\Development\\oscillare\\src\\static"

wsApp :: MVar ServerState -> ServerApp
wsApp ss pc = do
  conn <- acceptRequest pc
  forkPingThread conn 30
  wsRecvMsg ss conn

wsRecvMsg :: MVar ServerState -> Connection -> IO ()
wsRecvMsg ss conn = do
  message <- receiveData conn
  print message
  maybe (return ()) (applyMessage ss . parseMessage) $ decode message
  wsRecvMsg ss conn

udpRecvMsg :: (T.Transport t) => MVar ServerState -> ReaderT t IO ()
udpRecvMsg ss = do
  mapReaderT (mapM_ (applyMessage ss . parseMessage) =<<) OSC.recvMessages
  udpRecvMsg ss

parseMessage :: OSC.Message -> (Address, [OSC.Datum])
parseMessage (OSC.Message addr args) =
  (strToAddress . drop 1 . split '/' $ pack addr, args)

data Address =
  Mood
  | Time
  | Prog
  | Midi
  | Input ByteString
  | None ByteString

strToAddress :: [ByteString] -> Address
strToAddress ["mood"] = Mood
strToAddress ["tempo"] = Time
strToAddress ["progs"] = Prog
strToAddress ["input", i] = Input i
strToAddress ("midi":_) = Midi
strToAddress bs = None $ concat bs

applyMessage :: MVar ServerState -> (Address, [OSC.Datum]) -> IO ()
applyMessage mss (Prog, [OSC.datum_floating -> Just y, OSC.datum_floating -> Just x, OSC.datum_floating -> Just on]) = do
  ss <- readMVar mss
  modifyMVar_ (ss ^. ts) $ \ts' -> do
    (log, ts'') <- runStateT (doProg (truncate x) (truncate y) on) ts'
    print log
    return ts''

applyMessage mss (Mood, [mood]) = do
  ss <- readMVar mss
  modifyMVar_ (ss ^. ts) $ \ts' -> do
    (log, ts'') <- runStateT (doMood mood) ts'
    print log
    return ts''

applyMessage mss (Time, [OSC.datum_floating -> Just time]) = do
  ss <- readMVar mss
  modifyMVar_ (ss ^. ts) $ runReaderT (changeTempo time)

applyMessage mss (Input inputName, xs) = do
  ss <- readMVar mss
  modifyMVar_ (ss ^. ts) $ \ts' -> do
    (log, ts'') <- runStateT (doInput inputName xs) ts'
    print log
    return ts''

applyMessage mss (Midi, t:(fmap fromIntegral . OSC.datum_int32 -> Just c):(fmap ((/(127 :: Double)). fromIntegral) . OSC.datum_int32 -> Just v):[]) = do
  ss <- readMVar mss
  let tsf = maybe id (\c' -> inputs . at c' ?~ v) (lookup c launchControlXLMap)
  modifyMVar_ (ss ^. ts) $ pure . tsf


applyMessage _ (None bs, dat) =
  print ("Invalid Message Args : " ++ show bs ++ ", " ++ show dat)

applyMessage _ (_, dat) =
  print ("Invalid Args : " ++ show dat)

maybeTuple :: (Maybe (a, a'), Maybe (b, b')) -> Maybe (a, b)
maybeTuple (a, b) = do
  a' <- a
  b' <- b
  Just (fst a', fst b')

doMood :: Monad m => OSC.Datum -> StateT TempoState m String
doMood (OSC.ASCII_String "energetic") =
  setProgs [ pSideEmitter "a" (VolumeInput, Identity 2) (KickInput, Identity 10) (0.2 :: Double) ((* 10) . sinMod) ((-30) :: Double)
           , (pSine "b" (* 1) (1 :: Double) (1 :: Double))
           ]

doMood (OSC.ASCII_String "chill") =
  setProgs [ pText "ba" ("Chillaxed" :: String)
           , pSine "bb" (* 1) (1 :: Double) (1 :: Double)
           ]

doMood _ = return "Invalid Mood"

doInput :: Monad m => ByteString -> [ OSC.Datum ] -> StateT TempoState m String
doInput inputName [(OSC.Float val)] = (inputs %= insert inputName (float2Double val)) >> return (unpack inputName ++ ": " ++ show val)
doInput inputName ((OSC.Float valx):(OSC.Float valy):_) = (inputs %= insert (append inputName (pack "x")) (float2Double valx)) >> (inputs %= insert (append inputName (pack "y")) (float2Double valy)) >> return (unpack inputName ++ ": " ++ show valx ++ ", " ++ show valy)
doInput _ _ = return "Invalid value"

progMap :: Map Int Program
progMap = fromList [ (0, pAudioData "aa" 1 AudioTexInput)
                   , (1, pInput "ab" CameraTexInput |+| pEdges 0 |+| pBrightness 2 1)
                   , (2, pLines "ac" (KickInput, Identity 0.1) 0.2 |+| pMirror)
                   , (3, pFlocking "ad" (KickInput, Identity 80) 1.0 64 |+| pFade 0.4)
                   , (5, pShapes "ae" ((+ 3) . (* 6) . sinMod') 0.2 0.2 |+| pRepeat 9)
                   , (6, pStringTheory "af" (* 3) sinMod' 1 |+| pRepeat 3 |+| pFilter (* 1))
                   , (7, pShapes "ag" ((+ 3) . (* 6) . sinMod') 0.4 0.2 |+| pLittlePlanet)
                   ]

effMap :: Map Int [Effect]
effMap = fromList [ (0, [pFade 0.94])
                 , (1, [pFade 0.7, pFade (VolumeInput, Identity 1)])
                 , (2, [ pLittlePlanet ])
                 , (3, [ pFilter (* 1) ])
                 , (4, [ pFilter (VolumeInput, Identity 2) ])
                 , (5, [ pScale' (KickInput, Identity 0.07), pScale' (-0.015), pFade 0.98  ])
                 , (6, [ pBlur 27 ])
                 , (7, [ pEdges 0 ])
                 , (8, [ pLumidots ])
                 ]

doProg :: Monad m => Int -> Int -> Float -> StateT TempoState m String
doProg x 0 1 = do
  let prog = lookup x progMap
  case prog of
    Just p -> setAddProg p
    Nothing -> return ""
doProg x 0 0 = do
  let prog = lookup x progMap
  case prog of
    Just p -> do
      a <- zoom exec $ remProg (programSlot p)
      a' <- setProg' (Program (programSlot p) Blank)
      return (a ++ a')
    Nothing -> return ""

doProg x 1 1 = do
  let effs = lookup x effMap
  case effs of
    Just effs' -> do
      exec . effects %= unionBy (\a b -> fst a == fst b) [(x, effs')]
      st <- get
      return $ show $ view (exec . effects) st
    Nothing -> return ""
doProg x 1 0 = do
  let effs = lookup x effMap
  case effs of
    Just effs' -> do
      exec . effects %= deleteBy (\a b -> fst a == fst b) (x, effs')
      st <- get
      return $ show $ view (exec . effects) st
    Nothing -> return ""

doProg _ _ _ = return ""

-- setTriggered :: Monad m => ByteString -> StateT TempoState m String
-- setTriggered ps = do
--   ts <- get
--   patt.ix "s" %= _
--   ts' <- get
--   return $ mconcat $ execWriterT (logProg ts')
--   -- let
--   --   set' ps =
--   --     maybe ps (lookup "s" -> (Program slot (SlottableProgram (BaseProgram TriggeredPassthrough pus es))))
--   --     Program slot (SlottableProgram (BaseProgram TriggeredPassthrough pus es))
--   -- ts <- get

-- setTriggered' :: Program -> Program
-- setTriggered' (Program s (SlottableProgram (BaseProgram TriggeredPassthrough (UniformStringValue ))))


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
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import Data.Aeson (decode)
import Data.ByteString.Char8 (ByteString, concat, pack, readInt, split)
import Data.Char
import Data.Map (Map, fromList, (!), lookup)
import Data.Maybe
import Data.Text.Lazy.Encoding (encodeUtf8)
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

serverState :: MVar TempoState -> IO (MVar ServerState)
serverState mts = newMVar $ ServerState mts []

serve :: (T.Transport t) => MVar TempoState -> ReaderT t IO ()
serve mts = do
  ss <- liftIO $ serverState mts
  _ <- liftIO $ run 8000 $ websocketsOr defaultConnectionOptions (wsApp ss) staticPage
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
  | None ByteString

strToAddress :: [ByteString] -> Address
strToAddress ["mood"] = Mood
strToAddress ["tempo"] = Time
strToAddress ["progs"] = Prog
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
  setProgs [ ptTriggered "s" ("a b" :: String) (KickInput, (0.9 :: Double)) |+| pFade (0.98 :: Double)
           , pSideEmitter "a" (VolumeInput, (2 :: Double)) (KickInput, (10 :: Double)) (0.2 :: Double) ((* 10) . sinMod) ((-30) :: Double)
           , (pSine "b" (* 1) (1 :: Double) (1 :: Double))
           ]

doMood (OSC.ASCII_String "chill") =
  setProgs [ ptTriggered "s" ("a b" :: String) (KickInput, (0.9 :: Double)) |+| pFade (0.98 :: Double)
           , pText "a" ("Chillaxed" :: String)
           , pSine "b" (* 1) (1 :: Double) (1 :: Double)
           ]

doMood _ = return "Invalid Mood"

progMap :: Map Int Program
progMap = fromList [ (0, pAudioData "a" 1 (AudioTexInput, 5) |+| pFade 0.97)
                   , (1, pInput "b" (CameraTexInput, 1) |+| pEdges |+| pBrightness 2 |+| pFade (VolumeInput, 0.98))
                   , (2, pLines "c" (KickInput, 0.1) 0.2 |+| pMirror)
                   , (3, pFlocking "d" (KickInput, 80) 1 64 |+| pFade 0.98)
                   , (4, pDots "e" 1 (EqTexInput, 6) |+| pMirror)
                   , (5, pShapes "f" ((+ 3) . (* 6) . sinMod') 0.2 0.2 |+| pRepeat 9)
                   , (6, pStringTheory "g" (* 3) sinMod' 1 1 |+| pRepeat 3 |+| pFilter (* 1))
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


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Server where

import Prelude hiding (concat)

import Control.Concurrent
import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Aeson (decode)
import Data.ByteString.Char8 (ByteString, concat, pack, readInt, split)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe
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
  staticApp $ defaultWebAppSettings "C:\\Users\\Ulysses\\Development\\oscillare\\src\\static"

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
  | Prog ByteString ByteString
  | None ByteString

strToAddress :: [ByteString] -> Address
strToAddress ["mood"] = Mood
strToAddress bs = None $ concat bs

applyMessage :: MVar ServerState -> (Address, [OSC.Datum]) -> IO ()
applyMessage mss ((Prog x y), [toggled]) = do
  print $ concat [x, ", ", y, ", ", pack $ show toggled]
  let modifier t = if OSC.datum_floating toggled == Just 1.0 then ((:) t) else filter (/= t)
  maybe (return ()) (\t -> modifyMVar_ mss (return . over toggles (modifier t)))
    $ maybeTuple (readInt x, readInt y)
  readMVar mss >>= print . view toggles

applyMessage mss (Mood, [mood]) = do
  ss <- readMVar mss
  modifyMVar_ (ss ^. ts) $ \ts' -> do
    (log, ts'') <- runStateT (doMood mood) ts'
    print log
    return ts''

applyMessage _ (None bs, dat) =
  print ("Invalid Message Args : " ++ show bs ++ ", " ++ show dat)

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

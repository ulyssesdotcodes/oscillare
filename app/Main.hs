module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Fixed
import Data.Time.Clock
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T
import qualified Data.ByteString.Char8 as BS


type MessageQueue = Chan Message

ostr = ASCII_String . BS.pack

main = do
  conn <- openUDP "127.0.0.1" 9001
  forkIO $ runReaderT (sync []) conn
  flip runReaderT conn $ do
    _ <- sendScale
    sendBoth

sync :: (T.Transport t) => [String] -> ReaderT t IO ()
sync addrs = do
  now <- liftIO getCurrentTime
  let floatNow = realToFrac $ utctDayTime now
  mapM_ (\a -> sendOSC $ Message "/progs/uniform" [ostr a, ostr "time", float $ mod' 1.0 floatNow]) addrs
  sendOSC $ Message "/shader/uniform/time" [float floatNow]
  liftIO $ threadDelay 16000
  sync addrs

sendBoth :: (T.Transport t) => ReaderT t IO ()
sendBoth = do
  sendSine
  liftIO $ threadDelay 1000000
  sendLine
  liftIO $ threadDelay 1000000
  sendBoth

sendSine :: (T.Transport t) => ReaderT t IO ()
sendSine = do
  sendOSC $ Message "/progs" [ostr "p1", ostr "sine"]

sendLine :: (T.Transport t) => ReaderT t IO ()
sendLine =
  sendOSC $ Message "/progs" [ostr "p1", ostr "line_down"]

sendScale :: (T.Transport t) => ReaderT t IO ()
sendScale = do
  sendOSC $ Message "/shader" [ostr "scale"]
  sendOSC $ Message "/shader/base" [ostr "p1"]

sendTime :: (T.Transport t) => ReaderT t IO ()
sendTime = do
  now <- liftIO getCurrentTime
  let floatNow = realToFrac $ utctDayTime now
  sendOSC $ Message "/shader/uniform/time" [float floatNow]
  sendOSC $ Message "/progs/uniform" [ostr "p1", ostr "time", float $ mod' 1.0 floatNow ]
  sendOSC $ Message "/progs/uniform" [ostr "p1", ostr "scale", float . sin $ mod' 1.0 floatNow]





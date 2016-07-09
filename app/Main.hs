module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Time.Clock
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T
import qualified Data.ByteString.Char8 as BS


type MessageQueue = Chan Message

main = do
  conn <- openUDP "127.0.0.1" 9001
  forkIO $ sync conn
  keepSending conn sendBoth

ostr = ASCII_String . BS.pack

keepSending :: UDP -> ReaderT UDP IO () -> IO ()
keepSending t r = do
  runReaderT r t
  threadDelay 1000000
  print "next"
  keepSending t r

sync :: UDP -> IO ()
sync t = do
  runReaderT sendTime t
  threadDelay 16000
  sync t

sendBoth :: (T.Transport t) => ReaderT t IO ()
sendBoth = do
  _ <- sendSine
  liftIO $ threadDelay 1000000
  -- _ <- sendLine
  return ()

sendSine :: (T.Transport t) => ReaderT t IO ()
sendSine =
  sendOSC $ Message "/shader" [ostr "sine"]

sendLine :: (T.Transport t) => ReaderT t IO ()
sendLine =
  sendOSC $ Message "/shader" [ostr "line_down"]

sendTime :: (T.Transport t) => ReaderT t IO ()
sendTime = do
  now <- liftIO getCurrentTime
  sendOSC $ Message "/shader/uniform/time" [float . realToFrac $ utctDayTime now]




module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T
import qualified Data.ByteString.Char8 as BS


type MessageQueue = Chan Message

main = do
  conn <- openUDP "127.0.0.1" 9001
  -- T.sendOSC conn $ Message "/shaders/0" [ASCII_String $ BS.pack "sine"]
  keepSending conn sendSine

keepSending :: UDP -> ReaderT UDP IO () -> IO ()
keepSending t r = do
  runReaderT r t
  threadDelay 1000000
  print "next"
  keepSending t r

sendSine :: (T.Transport t) => ReaderT t IO ()
sendSine = do
  let msg =  Message "/shaders/0" [ASCII_String $ BS.pack "sine"]
  return $ print msg
  return $ print "hi"
  sendOSC msg

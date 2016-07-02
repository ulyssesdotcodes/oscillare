module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans.Maybe
import Sound.OSC
import Graphics.Luminance

import Graphics
import Osc
import OscParser

import qualified Graphics.UI.GLFW as G

type MessageQueue = Chan Message

server :: String
server = "127.0.0.1"

receiveMessageTransport :: IO UDP -> MessageQueue -> IO ()
receiveMessageTransport t mChan = withTransport t $ forever $ do
  msg <- waitMessage
  liftIO $ writeChan mChan msg

receiveMessages :: MessageQueue -> IO ()
receiveMessages = receiveMessageTransport $ udpServer server 3333

handleUDPMessages :: MessageQueue -> IO ()
handleUDPMessages mChan = do
  msg <- readChan mChan
  print msg
  handleUDPMessages mChan

infinirun :: IO ()
infinirun = do
  threadDelay 16
  infinirun

main :: IO ()
main = do
  messageChan <- newChan
  _ <- forkIO $ handleUDPMessages messageChan
  fragV <- newMVar (ShaderProgram "app/shaders/basic.frag" ["i_time"] [0.5])
  thread <- forkOS $ do
    successfulInit <- G.init
    if successfulInit then do
      G.windowHint $ G.WindowHint'ContextVersionMajor 3
      G.windowHint $ G.WindowHint'ContextVersionMinor 3
      window <- runMaybeT $ do
        monitor <- MaybeT G.getPrimaryMonitor
        vm <- (MaybeT . G.getVideoMode) monitor
        w <- MaybeT $ G.createWindow (G.videoModeWidth vm) (G.videoModeHeight vm) "Simple example" Nothing Nothing
        return (vm, w)
      G.makeContextCurrent (fmap snd window)
      G.swapInterval 1
      -- maybe (print "No window") (run . snd) window
      maybe (print "No window") ((run fragV) . snd) window
      G.terminate
    else
      print "Couldn't init"
  threadDelay 10000000
  putMVar fragV (ShaderProgram "app/shaders/sin.frag" [] [])
  infinirun

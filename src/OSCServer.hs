module OSCServer where

import LambdaDesigner.Op
import LambdaDesigner.JSONOutput
import LambdaDesigner.Lib

import Control.Monad.Trans.State
import Data.IORef
import Data.List
import Data.Trie
import Sound.OSC.Transport.FD

import qualified Sound.OSC as OSC


run :: (Op a) => IORef Messages -> [Tree a] -> IO ()
run state tree = run2 state tree ([] :: [Tree TOP])

run2 :: (Op a, Op b) => IORef Messages -> [Tree a] -> [Tree b] -> IO ()
run2 state tas tbs = do
  state' <- readIORef state
  let state'' = compile tas tbs state'
      msgs' = [OSC.Message ("/json") [OSC.ASCII_String $ makeMessages state'']]
  writeIORef state state''
  conn <- OSC.openUDP "127.0.0.1" 9002
  sendMessages conn msgs'
  close conn
  return ()

topRunner :: IO (Tree TOP -> IO ())
topRunner = do init <- newIORef mempty
               return $ run init . (:[]) . outT

sendMessages :: OSC.UDP -> [OSC.Message] -> IO ()
sendMessages conn ms = sendOSC conn $ OSC.Bundle 0 $ ms
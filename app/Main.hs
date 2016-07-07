module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans.Maybe
import Sound.OSC

import Stream


type MessageQueue = Chan Message

main =
  Stream.start (Backend print (\_ _ _ -> return ()))

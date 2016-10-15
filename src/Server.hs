module Server where

import Control.Concurrent
import Control.Monad.Trans.Reader
import Sound.OSC as OSC
import Sound.OSC.Transport.FD as T

type MessageQueue = Chan OSC.Message

server :: (T.Transport t) => ReaderT t IO ()
server = do
  mapReaderT (mapM_ processMessage =<<) OSC.recvMessages
  server

processMessage :: Message -> IO ()
processMessage = print

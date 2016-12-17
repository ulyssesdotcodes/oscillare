{-# LANGUAGE ViewPatterns #-}

module Midi where

import Pattern
import State

import Prelude hiding (lookup)

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString.Char8 hiding (map, foldr)
import Data.Map.Strict hiding (map, foldr)
import qualified Sound.PortMidi as PM

launchControlXLMap :: Map Int ByteString
launchControlXLMap = fromList $ map (\n -> (n, pack $ "pot" ++ show (n - 13))) [13..37]

midi :: String -> Pattern Double
midi (pack -> s) = reader $ (:[]) . findWithDefault 0 s . pInputs

initMidi :: MVar TempoState -> IO ()
initMidi mt = do
  initErr <- PM.initialize
  case initErr of
    PM.NoError -> do
      err <- bracket (PM.openInput 3) (either PM.close return) (either (readMsgs mt) return)
      print err
    _ -> do
      print initErr

  return ()

readMsgs :: MVar TempoState -> PM.PMStream -> IO PM.PMError
readMsgs mt stream = do
  events <- PM.readEvents stream
  case events of
    Right err -> return err
    Left msgs -> do
      takeMVar mt >>= execStateT (sequence $ map (writeMessage . PM.decodeMsg . PM.message) msgs) >>= putMVar mt
      return PM.NoError

      -- modifyMVar mt $ flip (foldr (execStateT . mapStateT (_1 .~ PM.NoError) . writeMessage . PM.decodeMsg . PM.message)) msgs

writeMessage :: Monad m => PM.PMMsg -> StateT TempoState m ()
writeMessage (PM.PMMsg _ (flip lookup launchControlXLMap . fromIntegral -> Just chan) val) =
  inputs . at chan . _Just .= (fromIntegral val) / (128 :: Double)
writeMessage _ = state $ \s -> ((),s)

-- someFunc :: IO ()
-- someFunc = do
--   err <- PM.initialize
--   print err
--   devInfo <- PM.getDeviceInfo 3
--   print devInfo
--   stream <- PM.openInput 3
--   print stream
--   case stream of
--     Left p -> do
--       readOne p
--       err' <- PM.close p
--       print err'
--     Right e -> print e
--   err'' <- PM.terminate
--   print err''
--   return ()

-- readOne p = do
--   events <- PM.readEvents p
--   case events of
--     Left msg -> print (PM.decodeMsg (PM.message $ head msg)) >>= \_ -> return ()
--     Right err -> readOne p

-- repeatIOAction :: Int -> IO () -> IO ()     -- Inputs: integer and IO. Outputs: IO 
-- repeatIOAction 0 _ = return ()              -- exit recursive loop here
-- repeatIOAction n action = do
--     action                                  -- action to perform
--     repeatIOAction (n-1) action             -- decrement n to make it recursive

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}

module Server where

import Control.Concurrent (forkIO, killThread)
import Control.Monad
import Data.Aeson
import GHC.Generics
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.FilePath.Posix
import System.FSNotify
import System.IO (stdout, hFlush, hSetBuffering, hGetContents, hGetLine, hPutStrLn, BufferMode(..), Handle)
import System.CPUTime
import Text.Printf

import Debug.Trace

import qualified Data.ByteString.Char8 as BS

run :: IO ()
run = do
  getArgs >>= \case
    [pathToWatch] -> watchPath pathToWatch
    _             -> error "Error: Name a file"
  runInterpreter (evalLD "r $ rectangle (float 0.2) (float 0.2)")
    >>= putStrLn
    .   show

watchPath :: FilePath -> IO ()
watchPath path = do
  wmgr <- startManagerConf (WatchConfig (Debounce 3) 1 False)
  thread <- forkIO (servePath path wmgr)
  putStrLn "Press enter to exit"
  void getLine
  stopManager wmgr
  killThread thread

servePath :: FilePath -> WatchManager -> IO ()
servePath path wmgr = withSocketsDo $ do
  sock <- listenOn $ PortNumber 5959
  putStrLn $ "Listening on 5959"
  sockHandler path sock wmgr

evalLD :: FilePath -> InterpreterT IO String
evalLD file = do
  loadModules [file]
  setImports [takeBaseName file]
  eval "network"

sockHandler :: FilePath -> Socket -> WatchManager -> IO ()
sockHandler path sock wmgr = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  void . forkIO $ watcher path handle wmgr
  putStrLn "Reopen"
  sockHandler path sock wmgr

watcher :: FilePath -> Handle -> WatchManager -> IO ()
watcher path handle wmgr = do
  BS.hPutStrLn handle "I gotcha"
  let update = do
        putStrLn "Test 2"
        start <- getCPUTime
        msg <- getCodeOrError path
        end <- getCPUTime
        printf "Updating took %0.3f sec\n" (((fromIntegral (end - start)) / (10^12)) :: Double) 
        BS.hPutStrLn handle (BS.pack . show $ encode $ toJSON msg)
  let onChange = \case
        Modified p _ -> do
          putStrLn p
          update
          return ()
        _            -> return ()
  update
  putStrLn "This should only run once"
  _ <- watchDir wmgr (takeDirectory path) (const True) onChange
  _ <- hGetLine handle
  return ()

-- Thank you Sean Lee (github.com/sleexyz) for this

data Msg = Err String
         | Code String
         deriving (Show, Generic, ToJSON, FromJSON)

getCodeOrError :: FilePath -> IO Msg
getCodeOrError path = do
  unsafeRunInterpreterWithArgs ["-package-db C:\\sr\\snapshots\\36302ab3\\pkgdb", "-package-db ./.stack-work/install/1d0d0160/pkgdb", "-no-user-package-db"] (evalLD path) >>= return . \case
    Left err -> case err of
      UnknownError str    -> Err str
      WontCompile  errors -> Err . mconcat $ errMsg <$> errors
      NotAllowed   str    -> Err str
      GhcException str    -> Err str
    Right str -> Code str
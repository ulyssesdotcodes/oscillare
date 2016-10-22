{-# LANGUAGE TemplateHaskell #-}

module State where


import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Map.Strict (Map, insert, foldMapWithKey)
import Data.Time.Clock
import Sound.OSC

import Program


data TempoState = TempoState { _conn :: UDP, _patt :: Map ByteString Program, _start :: DiffTime, _cycleLength :: DiffTime, _prev :: Double, _current :: Double }

makeLenses ''TempoState

setProg :: Monad m => Program -> ReaderT TempoState m TempoState
setProg p = reader $ over patt (insert (programSlot p) p)

logProg :: Monad m => TempoState -> WriterT String m TempoState
logProg ts = do
  tell $ foldr ((++) . (\n -> "\n" ++ n) . show) [] $ view patt ts
  return ts

setProg' :: Monad m => Program -> StateT TempoState m String
setProg' p = do
  modify (runReader (setProg p))
  get >>= execWriterT . logProg

setProgs :: Monad m => [Program] -> StateT TempoState m String
setProgs ps = mapStateT (fmap (first last)) $ mapM setProg' ps



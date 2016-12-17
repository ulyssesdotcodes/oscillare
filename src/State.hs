{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module State where


import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.ByteString.Char8 (ByteString, pack, unpack, append)
import Data.List (union)
import Data.Map.Strict (Map, insert, foldMapWithKey, findWithDefault)
import Data.Time.Clock
import Sound.OSC

import Pattern
import Program
import Uniform

data TempoState = TempoState { _conn :: UDP
                             , _patt :: Map ByteString Program
                             , _start :: DiffTime
                             , _cycleLength :: DiffTime
                             , _prev :: Double
                             , _current :: Double
                             , _exec :: Exec
                             , _inputs :: Map ByteString Double
                             , _lastMessages :: [Message]
                             }

makeLenses ''TempoState

instance Effectable TempoState where
  (|+|) ts e = ts & exec %~ (|+| e)

setProg :: Monad m => Program -> ReaderT TempoState m TempoState
setProg p = reader $ over patt (insert (programSlot p) p)

logProg :: Monad m => TempoState -> WriterT String m TempoState
logProg ts = do
  tell $ foldr ((++) . (\n -> "\n" ++ n) . show) [] $ view patt ts
  tell $ show $ view exec ts
  return ts

setProg' :: Monad m => Program -> StateT TempoState m String
setProg' p = do
  modify (runReader (setProg p))
  get >>= execWriterT . logProg

setAddProg :: Monad m => Program -> StateT TempoState m String
setAddProg p = do
  setProg' p
  exec %%= execStateT (addProg (programSlot p))

setProgs :: Monad m => [Program] -> StateT TempoState m String
setProgs ps = mapStateT (fmap (first last)) $ mapM setProg' ps

changeTempo :: Monad m => Double -> ReaderT TempoState m TempoState
changeTempo t = reader $ set cycleLength (fromRational $ toRational (240 / t))

modProgs :: Monad m => ([ByteString] -> [ByteString]) -> StateT TempoState m ()
modProgs f = exec.progs %= f

setExecKick :: (Monad m, FloatUniformPattern a) => a  -> StateT TempoState m ()
setExecKick d = exec.kick .= fPattern d

addProg :: Monad m => ByteString -> StateT Exec m String
addProg p = do
  progs %= union [append p (pack "0")]
  get >>= return . show

remProg :: Monad m => ByteString -> StateT Exec m String
remProg p = do
  progs %= filter (/= append p (pack "0"))
  get >>= return . show

(+++) :: String -> TempoState -> TempoState
(+++) p = execState $ zoom exec (addProg (pack p))

(++-) :: String -> TempoState -> TempoState
(++-) p = execState $ zoom exec $ remProg (pack p)

kk :: FloatUniformPattern a => a -> TempoState -> TempoState
kk k ts = flip execState ts $ zoom exec $ kick .= fPattern k

sps :: [String] -> Exec -> Exec
sps = (progs .~) . (fmap (pack . (++ "0")))

(|-|) :: TempoState -> TempoState
(|-|) ts = ts & exec . effects .~ [] & id %~ resetCache

resetCache :: TempoState -> TempoState
resetCache = lastMessages .~ []

midi :: String -> Pattern Double
midi (pack -> s) = reader $ (:[]) . findWithDefault 0 s . pInputs

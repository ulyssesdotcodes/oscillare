module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Fixed
import Data.Time.Clock
import Sound.OSC
import qualified Sound.OSC.Transport.FD as T
import qualified Data.ByteString.Char8 as BS

data Pattern a = Pattern { arc :: Float -> [a] }

ostr = ASCII_String . BS.pack

main = do
  conn <- openUDP "127.0.0.1" 9001
  flip runReaderT conn $ sendOSC $ Message "/connection" []


instance Functor Pattern where
  fmap f (Pattern a) = Pattern (\t -> f <$> a t)

instance Applicative Pattern where
  pure p = Pattern $ const [p]
  Pattern fs <*> Pattern ms = Pattern (\t -> fs t <*> ms t)


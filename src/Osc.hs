{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Osc where

import           BasePrelude hiding (readMaybe)
import           Data.Aeson
import           Data.Aeson.Types
import           Sound.OSC.Type

import           Data.Vector ((!?))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as Vector

foo :: BSL.ByteString
foo = "[\"#bundle\",{\"timestamp\":0.0},[\"/c_set\",3,4.5],[\"/n_free\",0]]"

(.!) :: FromJSON a => Array -> Int -> Parser a
v .! ix = case v !? ix of
  Just a ->
    parseJSON a
  Nothing ->
    fail ("No such index " <> show ix <> " in vector " <> show v)

-- | This differs from decode_datum in that it always parses numbers
-- to a Float.
instance FromJSON Datum where
  parseJSON (Object o) =
    parseBlob <|> parseMidi <|> parseTimestamp
    where
      parseBlob =
        Blob . BSL.pack <$> o .: "blob"
      parseMidi = do
        [p, q, r, s] <- o .: "midi"
        return (midi (p, q, r, s))
      parseTimestamp =
        TimeStamp <$> (o .: "timestamp")
  parseJSON whole@(Number _) =
    Float <$> parseJSON whole
  parseJSON whole@(String _) =
    string <$> parseJSON whole
  parseJSON invalid =
    typeMismatch "Datum" invalid

instance ToJSON Datum where
  toJSON d =
    case d of
      Blob b -> object [ ("blob", toJSON $ BSL.unpack b)]
      Midi m -> object [ ("midi", toJSON $ encodeMidi m)]
      TimeStamp t -> object ["timestamp" .= t]
      Int32 n -> toJSON n
      Int64 n -> toJSON n
      Float n -> toJSON n
      Double n -> toJSON n
      ASCII_String s -> toJSON $ BSC.unpack s
    where
      encodeMidi (MIDI p q r s) = map toJSON [p,q,r,s]

instance FromJSON Bundle where
  parseJSON (Array v) = do
    b' <- v .! 0
    timestamp <- v .! 1
    guard (b' == String "#bundle")
    Bundle <$> (timestamp .: "timestamp") <*> traverse parseMessages (toList (Vector.drop 2 v))
    where
      parseMessages (Array w) = do
        addr <- w .! 0
        Message <$> parseJSON addr <*> traverse parseJSON (toList (Vector.drop 1 w))
      parseMessages invalid =
        typeMismatch "Bundle' messages" invalid
  parseJSON invalid =
    typeMismatch "Bundle'" invalid

instance ToJSON Bundle where
  toJSON (Bundle t m) = do
    let b = toJSON ("#bundle" :: String)
    let t' = toJSON t
    let m' = map encodeMessage m
    toJSON (b : t' : m')
    where
      encodeMessage (Message a d) =
        let a' = toJSON a
            d' = map toJSON d
        in toJSON (a' : d')

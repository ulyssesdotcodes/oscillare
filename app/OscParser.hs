module OscParser where

import Data.ByteString.Char8
import Sound.OSC.Core

import Osc

data ParsedMessage =
  ShaderMessage Shader

data Shader = Shader
  { content :: String
  }

parse :: Message -> Maybe ParsedMessage
parse (Message address datum) =
  case (address, datum) of
    ("shader-string", (ASCII_String str):_) ->
      Just . ShaderMessage $ Shader $ unpack str
    _ ->
      Nothing

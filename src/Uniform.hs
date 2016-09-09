{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Uniform where

import Data.ByteString.Char8 (ByteString, pack)

import Pattern

data FloatValue
  = FloatInputValue FloatInput Double
  | FloatDoubleValue Double
data FloatInput = VolumeInput

floatInputText :: FloatInput -> ByteString
floatInputText VolumeInput = "volume"

data TexValue
  = TexInputValue TexInput Double
data TexInput = AudioTexInput | EqTexInput

texInputText :: TexInput -> ByteString
texInputText AudioTexInput = "audio_texture"
texInputText EqTexInput = "eq_texture"

type StringValue = ByteString

data UniformValue
  = UniformFloatValue FloatValue
  | UniformTexValue TexValue
  | UniformStringValue StringValue

data Uniform =  Uniform { name :: ByteString, value :: UniformValue }

timeUniform :: Pattern Uniform
timeUniform = uniformPattern "time" (UniformFloatValue . FloatDoubleValue <$> timePattern)

deltaUniform :: Pattern Uniform
deltaUniform = uniformPattern "delta" $ UniformFloatValue . FloatDoubleValue <$> deltaPattern

uniformPattern :: ByteString -> Pattern UniformValue -> Pattern Uniform
uniformPattern n = fmap (Uniform n)

class FloatUniformPattern a where
  fPattern :: a -> Pattern FloatValue

instance FloatUniformPattern Double  where
  fPattern = pure . FloatDoubleValue

instance FloatUniformPattern (Pattern Double)  where
  fPattern = fmap FloatDoubleValue

instance RealFloat a => FloatUniformPattern [a]  where
  fPattern = seqp . fmap (pure . FloatDoubleValue . realToFrac)

instance RealFloat a => FloatUniformPattern (a -> a)  where
  fPattern f = FloatDoubleValue . realToFrac . f . realToFrac <$> timePattern

class FloatPattern a where
  floatPattern :: a -> Pattern Double

instance FloatPattern Double where
  floatPattern = pure

instance FloatPattern Integer where
  floatPattern = pure . fromInteger

instance FloatPattern (Pattern Double) where
  floatPattern = id

instance FloatPattern (Double -> Double) where
  floatPattern f = f <$> timePattern

instance RealFloat a => FloatPattern [a] where
  floatPattern = seqp . fmap (pure . realToFrac)

instance FloatPattern a => FloatUniformPattern (FloatInput, a)  where
  fPattern (i, m) = FloatInputValue i <$> floatPattern m

class StringUniformPattern a where
  sPattern :: a -> Pattern StringValue

instance StringUniformPattern String where
  sPattern = pure . pack

instance StringUniformPattern [String] where
  sPattern = seqp . fmap (pure . pack)

class TexUniformPattern a where
  tPattern :: a -> Pattern TexValue

instance FloatPattern a => TexUniformPattern (TexInput, a) where
  tPattern (i, m) = TexInputValue i <$> floatPattern m

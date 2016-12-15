{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Uniform where

import Data.ByteString.Char8 (ByteString, pack)

import Pattern

data FloatValue
  = FloatInputValue FloatInput [Double]
  | FloatDoubleValue Double
  deriving Eq
data FloatInput =
  VolumeInput
  | KickInput
  | Slider1Input
  | Slider2Input
  | Slider3Input
  | Slider4Input
  | Position1InputX
  | Position1InputY
  deriving Eq

floatInputText :: FloatInput -> ByteString
floatInputText VolumeInput = "volume"
floatInputText KickInput = "kick"
floatInputText Slider1Input = "slider1"
floatInputText Slider2Input = "slider2"
floatInputText Slider3Input = "slider3"
floatInputText Slider4Input = "slider4"
floatInputText Position1InputX = "pos1x"
floatInputText Position1InputY = "pos1y"

data TexValue
  = TexInputValue TexInput [Double] deriving Eq
data TexInput =
  AudioTexInput
  | EqTexInput
  | CameraTexInput
  | BlankTex
  deriving Eq

texInputText :: TexInput -> ByteString
texInputText AudioTexInput = "audio_texture"
texInputText EqTexInput = "eq_texture"
texInputText CameraTexInput = "camera_texture"
texInputText BlankTex = "blank"

type StringValue = ByteString

data UniformValue
  = UniformFloatValue FloatValue
  | UniformTexValue TexValue
  | UniformStringValue StringValue
  deriving Eq

data Uniform =  Uniform { name :: ByteString, value :: UniformValue } deriving Eq

timeUniform :: Pattern Uniform
timeUniform = uniformPattern "time" $ (UniformFloatValue . FloatDoubleValue) <$$> timePattern

deltaUniform :: Pattern Uniform
deltaUniform = uniformPattern "delta" $ UniformFloatValue . FloatDoubleValue <$$> deltaPattern

uniformPattern :: ByteString -> Pattern UniformValue -> Pattern Uniform
uniformPattern n = ffmap (Uniform n)

class FloatUniformPattern a where
  fPattern :: a -> Pattern FloatValue

instance FloatUniformPattern Double  where
  fPattern = pure . (:[]) . FloatDoubleValue

instance FloatUniformPattern (Pattern Double)  where
  fPattern = ffmap FloatDoubleValue

instance RealFloat a => FloatUniformPattern [a]  where
  fPattern = seqp . fmap (pure . (:[]) . FloatDoubleValue . realToFrac)

instance RealFloat a => FloatUniformPattern (a -> a)  where
  fPattern f = FloatDoubleValue . realToFrac . f . realToFrac <$$> timePattern

class FloatPattern a where
  floatPattern :: a -> Pattern Double

instance FloatPattern Double where
  floatPattern = mempty

instance FloatPattern Integer where
  floatPattern = pure . (:[]) . fromInteger

instance FloatPattern (Pattern Double) where
  floatPattern = id

instance FloatPattern (Double -> Double) where
  floatPattern f = f <$$> timePattern

instance FloatPattern a => FloatUniformPattern (FloatInput, [a])  where
  fPattern (i, m) = ((:[]) . FloatInputValue i) <$> (mconcat $ fmap floatPattern m)

class StringUniformPattern a where
  sPattern :: a -> Pattern StringValue

instance StringUniformPattern String where
  sPattern = pure . (:[]) . pack

instance StringUniformPattern [String] where
  sPattern = seqp . fmap (pure . (:[]) . pack)

class TexUniformPattern a where
  tPattern :: a -> Pattern TexValue

instance FloatPattern a => TexUniformPattern (TexInput, [a]) where
  tPattern (EqTexInput, d:[]) = TexInputValue EqTexInput . (:[]) <$$> floatPattern d
  tPattern (_, _) = pure . (:[]) $ TexInputValue BlankTex []

instance TexUniformPattern TexInput where
  tPattern EqTexInput = pure . (:[])$ TexInputValue BlankTex []
  tPattern t = pure . (:[]) $ TexInputValue t []

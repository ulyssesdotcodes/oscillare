{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Uniform where

import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad.Identity

import Pattern

data FloatValue
  = FloatInputValue FloatInput [Double]
  | FloatDoubleValue Double
  deriving Eq
data FloatInput =
  VolumeInput
  | KickInput
  | PassInput
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
floatInputText PassInput = "pass"
floatInputText Slider1Input = "slider1"
floatInputText Slider2Input = "slider2"
floatInputText Slider3Input = "slider3"
floatInputText Slider4Input = "slider4"
floatInputText Position1InputX = "pos1x"
floatInputText Position1InputY = "pos1y"

data TexValue
  = TexInputValue TexInput [Double] deriving Eq
data TexInput =
  AcidPalette
  | AudioTexInput
  | CameraTexInput
  | EqTexInput
  | EclecticPalette
  | PrayerFlagPalette
  | PurpleBluePalette
  | RibbonPalette
  | BlankTex
  deriving Eq

texInputText :: TexInput -> ByteString
texInputText AcidPalette = "acid_palette"
texInputText AudioTexInput = "audio_texture"
texInputText CameraTexInput = "camera_texture"
texInputText EqTexInput = "eq_texture"
texInputText EclecticPalette = "eclectic_palette"
texInputText PrayerFlagPalette = "prayer_flag_palette"
texInputText PurpleBluePalette = "purple_blue_palette"
texInputText RibbonPalette = "ribbon_palette"
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
  floatPattern = pure . (:[])

instance FloatPattern Integer where
  floatPattern = pure . (:[]) . fromInteger

instance FloatPattern (Pattern Double) where
  floatPattern = id

instance FloatPattern (Double -> Double) where
  floatPattern f = f <$$> timePattern

instance FloatPattern a => FloatUniformPattern (FloatInput, Identity a)  where
  fPattern (i, Identity m) = ((:[]) . FloatInputValue i) <$> floatPattern m

instance (FloatPattern a, FloatPattern b) => FloatUniformPattern (FloatInput, (a, b))  where
  fPattern (i, (a, b)) = ((:[]) . FloatInputValue i) <$> mconcat [floatPattern a, floatPattern b]

instance (FloatPattern a, FloatPattern b, FloatPattern c) => FloatUniformPattern (FloatInput, (a, b, c))  where
  fPattern (i, (a, b, c)) = ((:[]) . FloatInputValue i) <$> mconcat [floatPattern a, floatPattern b, floatPattern c]

instance (FloatPattern a, FloatPattern b, FloatPattern c, FloatPattern d) => FloatUniformPattern (FloatInput, (a, b, c, d))  where
  fPattern (i, (a, b, c, d)) = ((:[]) . FloatInputValue i) <$> mconcat [floatPattern a, floatPattern b, floatPattern c, floatPattern d]

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Uniform where

import Data.Text (Text)

import Pattern

data UniformValue = UniformFloat Double | UniformInput (InputType, Double)

data InputType = AudioTexture | Volume | EqTexture

data FloatInput = VolumeInput
data TexInput = AudioTexInput | EqTexInput

inputText :: InputType -> Text
inputText AudioTexture = "audio_texture"
inputText Volume = "volume"
inputText EqTexture = "eq_texture"

uf = UniformFloat
ui = UniformInput

zipui :: InputType -> Double -> UniformValue
zipui i f = UniformInput (i, f)

data Uniform =  Uniform { name :: Text, value :: UniformValue }

timeUniform :: Pattern Uniform
timeUniform = uniformPattern "time" (UniformFloat <$> timePattern)

deltaUniform :: Pattern Uniform
deltaUniform = uniformPattern "delta" $ UniformFloat <$> deltaPattern

uniformPattern :: Text -> Pattern UniformValue -> Pattern Uniform
uniformPattern n = fmap (Uniform n)

class FloatUniformPattern a where
  fuPattern :: a -> Pattern UniformValue

instance FloatUniformPattern Double where
  fuPattern = fmap uf . pure

instance FloatUniformPattern (Pattern Double) where
  fuPattern = fmap uf

instance FloatPattern a => FloatUniformPattern (FloatInput, a) where
  fuPattern (VolumeInput, f) = zipui Volume <$> floatPattern f

class TexUniformPattern a where
  tuPattern :: a -> Pattern UniformValue

instance FloatPattern a => TexUniformPattern (TexInput, a) where
  tuPattern (AudioTexInput, f) = zipui AudioTexture <$> floatPattern f
  tuPattern (EqTexInput, f) = zipui EqTexture <$> floatPattern f


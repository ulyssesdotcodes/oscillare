{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Program where

import Data.ByteString.Char8 (ByteString, pack)

import Pattern
import Uniform

type Slot = ByteString

data EffectType =
  Brightness
  | Fade
  | Filter
  | Repeat
  | Scale

data LayerType =
  Add
  | Mult

data BaseType =
  AudioData
  | Dots
  | Flocking
  | Sine
  | StringTheory

data Name =
  BaseName BaseType
  | EffName EffectType
  | LayerName LayerType

data Effect =
  Effect EffectType (Pattern Uniform)
data BaseProgram =
  BaseProgram BaseType (Pattern Uniform) [Effect]
data Slottable =
  SlottableProgram BaseProgram
  | Passthrough (Pattern StringValue)
  | Layer LayerType [Slot] [Effect]
data Program = Program Slot Slottable

programSlot :: Program -> Slot
programSlot (Program s _) = s

effectName :: EffectType -> ByteString
effectName Brightness = "brightness"
effectName Fade = "fade"
effectName Filter = "filter"
effectName Scale = "scale"
effectName Repeat = "repeat"

layerName :: LayerType -> ByteString
layerName Add = "add"
layerName Mult = "mult"

baseName :: BaseType -> ByteString
baseName AudioData = "audio_data"
baseName Dots = "dots"
baseName Flocking = "flocking"
baseName Sine = "sine"
baseName StringTheory = "string_theory"

progName :: Name -> ByteString
progName (BaseName b) = baseName b
progName (EffName e) = effectName e
progName (LayerName l) = layerName l

(|+|) :: Program -> Effect -> Program
(|+|) (Program s (SlottableProgram (BaseProgram b us es))) e = Program s $ SlottableProgram $ BaseProgram b us (e:es)
(|+|) (Program s (Layer l ss es)) e = Program s $ Layer l ss (e:es)
(|+|) (Program s (Passthrough ps)) _ = Program s (Passthrough ps)

baseProg :: String -> BaseType -> Pattern Uniform -> Program
baseProg s b us = Program (pack s) (SlottableProgram (BaseProgram b us []))

layer :: LayerType -> String -> [String] -> Program
layer l s ss = Program (pack s) (Layer l (pack <$> ss) [])

upf :: FloatUniformPattern f => ByteString -> f -> Pattern Uniform
upf t pu = uniformPattern t $ UniformFloatValue <$> fPattern pu

upt :: TexUniformPattern t => ByteString -> t -> Pattern Uniform
upt t pt = uniformPattern t $ UniformTexValue <$> tPattern pt

singleUEffect :: FloatUniformPattern f => EffectType -> f -> Effect
singleUEffect e f = Effect e $ upf (effectName e) f

pAudioData slot uVolume uData = baseProg slot AudioData $ (upf "volume" uVolume) `mappend` (upt "tex_audio" uData)
pDots slot uVolume uData = baseProg slot Dots $ (upf "invVolume" uVolume) `mappend` (upt "eqs" uData)
pFlocking slot uAlignment uCohesion uSeparation = baseProg slot Flocking $ mconcat [upf "delta" deltaPattern, upf "alignment" uAlignment, upf "cohesion" uCohesion, upf "separation" uSeparation, upf "time" timePattern]
pStringTheory slot uTimeMod uAngle uAngleDelta uXoff = baseProg slot StringTheory $ mconcat [upf "angle" uAngle, upf "angle_delta" uAngleDelta, upf "xoff" uXoff]
pSine slot uTimeMod uScale uAmplitude = baseProg slot Sine $ mconcat [upf "time" $ (uTimeMod :: (Double -> Double)) <$> timePattern, upf "scale" uScale, upf "amplitude" uAmplitude]

pBrightness u = singleUEffect Brightness u
pFade u = singleUEffect Fade u
pFilter u = singleUEffect Filter u
pScale u = singleUEffect Scale u
pRepeat u = singleUEffect Repeat u

pAdd = layer Add
pMult = layer Mult

pt :: StringUniformPattern a => String -> a -> Program
pt s sp = Program (pack s) (Passthrough $ sPattern sp)

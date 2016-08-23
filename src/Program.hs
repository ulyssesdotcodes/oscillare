{-# LANGUAGE OverloadedStrings #-}

module Program where

import Data.Text (Text, pack)

import Pattern
import Uniform

type Slot = Text

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
  | Passthrough (Pattern Slot)
  | Layer LayerType [Slot] [Effect]
data Program = Program Slot Slottable

programSlot :: Program -> Slot
programSlot (Program s _) = s

effectName :: EffectType -> Text
effectName Brightness = "brightness"
effectName Fade = "fade"
effectName Filter = "filter"
effectName Scale = "scale"
effectName Repeat = "repeat"

layerName :: LayerType -> Text
layerName Add = "add"
layerName Mult = "mult"

baseName :: BaseType -> Text
baseName AudioData = "audio_data"
baseName Dots = "dots"
baseName Flocking = "flocking"
baseName Sine = "sine"
baseName StringTheory = "string_theory"

progName :: Name -> Text
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

upf :: FloatUniformPattern f => Text -> f -> Pattern Uniform
upf t pu = uniformPattern t $ fuPattern pu

upt :: TexUniformPattern t => Text -> t -> Pattern Uniform
upt t pt = uniformPattern t $ tuPattern pt

singleUEffect :: FloatUniformPattern f => EffectType -> f -> Effect
singleUEffect e f = Effect e $ upf (effectName e) f

pAudioData slot uVolume uData = baseProg slot AudioData $ (upf "volume" uVolume) `mappend` (upt "tex_audio" uData)
pDots slot uVolume uData = baseProg slot Dots $ (upf "invVolume" uVolume) `mappend` (upt "eqs" uData)
--  pme Flocking "p1" ["delta", "alignment", "cohesion", "separation", "time"] [deltaPattern, uf . (* 0.1) <$> sinTimePattern, uf . (* 0.2) . sin . (* 3.1415) . (+ 0.5) <$> timePattern, ui <$> pure (Volume, 500), timePattern]
pStringTheory slot uTimeMod uAngle uAngleDelta uXoff = baseProg slot StringTheory $ mconcat [upf "angle" uAngle, upf "angle_delta" uAngleDelta, upf "xoff" uXoff]
pSine slot uTimeMod uScale uAmplitude = baseProg slot Sine $ mconcat [upf "time" $ (uTimeMod :: (Double -> Double)) <$> timePattern, upf "scale" uScale, upf "amplitude" uAmplitude]

pBrightness u = singleUEffect Brightness u
pFade u = singleUEffect Fade u
pFilter u = singleUEffect Filter u
pScale u = singleUEffect Scale u
pRepeat u = singleUEffect Repeat u

pAdd = layer Add
pMult = layer Mult


{-# LANGUAGE OverloadedStrings #-}

module Program where

import Data.Text (Text, pack)

import Pattern
import Uniform

type Slot = Text

data EffectType =
  Add
  | Brightness
  | Fade
  | Filter
  | Mult
  | Repeat
  | Scale

data BaseType =
  AudioData
  | Dots
  | Flocking
  | Sine
  | StringTheory

data Name =
  BaseName BaseType
  | EffName EffectType

data Effect = Effect EffectType (Pattern Uniform)
data BaseProgram = BaseProgram BaseType (Pattern Uniform) [Effect]
data Slottable =
  SlottableProgram BaseProgram
  | Passthrough (Pattern Slot)
data Program = Program Slot Slottable

programSlot :: Program -> Slot
programSlot (Program s _) = s

effectName :: EffectType -> Text
effectName Add = "add"
effectName Brightness = "brightness"
effectName Fade = "fade"
effectName Filter = "filter"
effectName Mult = "mult"
effectName Scale = "scale"
effectName Repeat = "repeat"

baseName :: BaseType -> Text
baseName AudioData = "audio_data"
baseName Dots = "dots"
baseName Flocking = "flocking"
baseName Sine = "sine"
baseName StringTheory = "string_theory"

progName :: Name -> Text
progName (BaseName b) = baseName b
progName (EffName b) = effectName b

(|+|) :: Program -> Effect -> Program
(|+|) (Program s (SlottableProgram (BaseProgram b us es))) e = Program s $ SlottableProgram $ BaseProgram b us (e:es)
(|+|) a _ = a

baseProg :: String -> BaseType -> Pattern Uniform -> Program
baseProg s b us = Program (pack s) (SlottableProgram (BaseProgram b us []))

upf :: FloatUniformPattern f => Text -> f -> Pattern Uniform
upf t pu = uniformPattern t $ fuPattern pu

upt :: TexUniformPattern t => Text -> t -> Pattern Uniform
upt t pt = uniformPattern t $ tuPattern pt

singleUEffect :: FloatUniformPattern f => EffectType -> f -> Effect
singleUEffect e f = Effect e $ upf (effectName e) f

pAudioData slot uVolume uData = baseProg slot AudioData $ (upf "volume" uVolume) `mappend` (upt "tex_audio" uData)
pDots slot uVolume uData = baseProg slot Dots $ (upf "invVolume" uVolume) `mappend` (upt "eqs" uData)
pStringTheory slot uAngle uAngleDelta uXoff = baseProg slot StringTheory $ mconcat [upf "angle" uAngle, upf "angle_delta" uAngleDelta, upf "xoff" uXoff]
pSine slot uTimeMod uScale uAmplitude = baseProg slot Sine $ mconcat [upf "time" $ (uTimeMod :: (Double -> Double)) <$> timePattern, upf "scale" uScale, upf "amplitude" uAmplitude]

-- pAdd  = noUProgram Add
pBrightness u = singleUEffect Brightness u
pFade u = singleUEffect Fade u
pFilter u = singleUEffect Filter u
-- -- Have this on the other branch: pFlocking next = pme Flocking
-- pMult = noUProgram Mult
pScale u = singleUEffect Scale u
pRepeat u = singleUEffect Repeat u


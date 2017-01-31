{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}


module Program where

import Control.Lens
import Data.ByteString.Char8 (ByteString, pack, append)
import Data.ByteString.Char8 as BSC

import Pattern
import Uniform

type Slot = ByteString

data EffectType =
  Blur
  | Brightness
  | ColorRepeat
  | Edges
  | Fade
  | Filter
  | Fire
  | HueShift
  | Lumidots
  | LittlePlanet
  | Mirror
  | NoiseDisplace
  | Overlay
  | PaletteCycle
  | PaletteMap
  | PaletteVoronoi
  | Repeat
  | Reverse
  | Rotate
  | Scale
  | Translate
  deriving Eq

data BaseType =
  AbstractSpiral
  | AudioData
  | CircleEmitter
  | Dots
  | FadeComp
  | Flocking
  | Image
  | InputTexBase
  | Lines
  | Shapes
  | SideEmitter
  | Sine
  | StringTheory
  | TextType
  | Video
  | Voronoi

  | Add
  | Displace
  | Mult
  | Over
  | Passthrough
  | TriggeredPassthrough

data Name =
  BaseName BaseType
  | EffName EffectType

effectName :: EffectType -> ByteString
effectName Blur = "blur"
effectName Brightness = "brightness"
effectName ColorRepeat = "color_repeat"
effectName Edges = "edge_detection"
effectName Fade = "fade"
effectName Filter = "filter"
effectName Fire = "fire"
effectName HueShift = "hue_shift"
effectName Lumidots = "lumidots"
effectName LittlePlanet = "little_planet"
effectName Mirror = "mirror"
effectName NoiseDisplace = "noise_displace"
effectName Overlay = "overlay"
effectName PaletteCycle = "palette_cycle"
effectName PaletteMap = "palette_map"
effectName PaletteVoronoi = "palette_voronoi"
effectName Repeat = "repeat"
effectName Reverse = "reverse"
effectName Rotate = "rotate"
effectName Scale = "scale"
effectName Translate = "translate"


baseName :: BaseType -> ByteString
baseName AbstractSpiral = "abstract_spiral"
baseName AudioData = "audio_data"
baseName CircleEmitter = "circle_emitter"
baseName Dots = "dots"
baseName FadeComp = "fade_comp"
baseName Flocking = "flocking"
baseName Image = "image"
baseName InputTexBase = "input_texture"
baseName Lines = "lines"
baseName SideEmitter = "emitter"
baseName Sine = "sine"
baseName Shapes = "shapes"
baseName StringTheory = "string_theory"
baseName TextType = "text"
baseName Video = "video"
baseName Voronoi = "voronoi"

baseName Add = "add"
baseName Displace = "displace"
baseName Mult = "mult"
baseName Over = "over"
baseName Passthrough = "pt"
baseName TriggeredPassthrough = "pt_triggered"

progName :: Name -> ByteString
progName (BaseName b) = baseName b
progName (EffName e) = effectName e

data Effect =
  Effect EffectType (Pattern Uniform)

instance Show Effect where
  show (Effect et _) = unpack $ effectName et

data BaseProgram =
  BaseProgram BaseType (Pattern Uniform) [Effect]

instance Show BaseProgram where
  show (BaseProgram bt _ es) = unpack (baseName bt) ++ " Effects:" ++ Prelude.concatMap ((++ ", ") . show) es

data Slottable =
  SlottableProgram BaseProgram
  | Blank deriving Show

data Program = Program {_slot :: Slot, _program :: Slottable }
  deriving Show

data Exec = Exec { _progs :: [ByteString]
                  , _kick :: Pattern FloatValue
                  , _effects :: [(Int, [Effect])]
                  }

instance Show Exec where
  show (Exec p _ es)= show p ++ show es

makeLenses ''Program
makeLenses ''Exec

class Effectable a where
  (|+|) :: a -> Effect -> a

instance Effectable Program where
  (Program s (SlottableProgram (BaseProgram b us es))) |+| e = Program s $ SlottableProgram $ BaseProgram b us (e:es)

instance Effectable Exec where
  (Exec ps k es) |+| e = Exec ps k ((-1, [e]):es)

programSlot :: Program -> Slot
programSlot (Program s _) = s

baseProg :: String -> BaseType -> Pattern Uniform -> Program
baseProg s b us = Program (pack s) (SlottableProgram (BaseProgram b us []))

upf :: FloatUniformPattern f => ByteString -> f -> Pattern Uniform
upf t pu = uniformPattern t $ UniformFloatValue <$$> fPattern pu

ups :: StringUniformPattern f => ByteString -> f -> Pattern Uniform
ups t pu = uniformPattern t $ UniformStringValue <$$> sPattern pu

upsWithBase :: StringUniformPattern f => ByteString -> f -> Pattern Uniform
upsWithBase t pu = uniformPattern t $ UniformStringValue . BSC.concat . (Prelude.map (`append` "0 ")) . (BSC.split ' ') <$$> sPattern pu

upt :: TexUniformPattern t => ByteString -> t -> Pattern Uniform
upt t pu = uniformPattern t $ UniformTexValue <$$> tPattern pu

singleUEffect :: FloatUniformPattern f => EffectType -> f -> Effect
singleUEffect e f = Effect e $ upf (effectName e) f

pAbstractSpiral slot uTime = baseProg slot AbstractSpiral $ (upf "time" uTime)
pAudioData slot uVolume uData = baseProg slot AudioData $ (upf "volume" uVolume) `mappend` (upt "tex_audio" uData)
pBlank slot = Program (pack slot) Blank
pCircleEmitter slot uLifespan uVel uRotation uPullback uTex = baseProg slot CircleEmitter $
  mconcat [ upf "delta" deltaPattern
          , upf "time" timePattern
          , upf "emitVel" uVel
          , upf "lifespan" uLifespan
          , upf "pullback" uPullback
          , upf "rotation" uRotation
          , upt "tex_audio" uTex
          ]
pDots slot uVolume uData = baseProg slot Dots $ (upf "volume" uVolume) `mappend` (upt "eqs" uData)
pFlocking slot uSeparation uMult uSpeed = baseProg slot Flocking $ mconcat [upf "alignment" uMult, upf "cohesion" ((* (0.4 :: Double)) <$$> floatPattern uMult), upf "separation" uSeparation, upf "time" timePattern, upf "delta" $ deltaPattern, upf "speed" uSpeed]
pImage slot uImage uClear = baseProg slot Image $ mconcat [ups "image" uImage, upf "clear_shade" uClear]
pInput slot uInput = baseProg slot InputTexBase $ upt "tex_input" uInput
pLines slot uWidth uSpacing = baseProg slot Lines $ (upf "width" uWidth) `mappend` (upf "spacing" uSpacing)
pShapes slot uSides uWidth uSize = baseProg slot Shapes $ mconcat [upf "sides" uSides, upf "width" uWidth, upf "size" uSize]
pSideEmitter slot uAmount uLifespan uSpread uYPos uXPos = baseProg slot SideEmitter $
  mconcat [ upf "emitterY" uYPos
          , upf "emitterX" uXPos
          , upf "emitVelX" uAmount
          , upf "emitVelY" $ (pure [(*)] <**> floatPattern uSpread) <**> ((+ (-0.5)) <$$> rand)
          , upf "delta" deltaPattern
          , upf "time" timePattern
          , upf "speed" deltaPattern
          , upf "lifespan" uLifespan
          ]
pSine slot uXPos uScale uAmplitude = baseProg slot Sine $ mconcat [upf "time" $ uXPos, upf "scale" uScale, upf "amplitude" uAmplitude]
pStringTheory slot uAngle uAngleDelta uXoff = baseProg slot StringTheory $ mconcat [upf "angle" uAngle, upf "angle_delta" uAngleDelta, upf "xoff" uXoff]
pText slot uText = baseProg slot TextType $ ups "text" uText
pVoronoi slot uTime = baseProg slot Voronoi $ upf "time" uTime
pVideo slot uPath uSpeed = baseProg slot Video $ mconcat [ups "video" uPath, upf "speed" uSpeed]

pt s sp = baseProg s Passthrough (upsWithBase "passthrough" sp)
pFadeComp s sp uIndex = baseProg s FadeComp $ upsWithBase "passthroughs" sp `mappend` upf "index" uIndex
ptTriggered s sp uTrig = baseProg s TriggeredPassthrough $ upsWithBase "passthroughs" sp `mappend` upf "trigger" uTrig
pAdd s sp = baseProg s Add (upsWithBase "references" sp)
pDisplace s sp = baseProg s Displace (upsWithBase "references" sp)
pMult s sp = baseProg s Mult (upsWithBase "references" sp)
pOver s sp = baseProg s Over (upsWithBase "references" sp)

pBlur u = singleUEffect Blur u
pBrightness uB uC = Effect Brightness $ mconcat [upf "brightness" uB, upf "contrast" uC]
pColorRepeat u uP = Effect ColorRepeat $ mconcat [upf "repeat" u, upt "palette" uP]
pEdges u = Effect Edges $ upf "overlay" u
pFade u = singleUEffect Fade u
pFilter u = singleUEffect Filter u
pFire u = singleUEffect Fire u
pHueShift u = singleUEffect HueShift u
pLittlePlanet = Effect LittlePlanet mempty
pLumidots = Effect Lumidots mempty
pMirror = Effect Mirror mempty
pNoiseDisplace u = Effect NoiseDisplace $ upf "displacement" u
pOverlay u = singleUEffect Overlay u
pPaletteCycle uPalette uOffset = Effect PaletteCycle $ mconcat [upt "palette" uPalette, upf "offset" uOffset]
pPaletteMap uPalette uOffset = Effect PaletteMap $ mconcat [upt "palette" uPalette, upf "offset" uOffset]
pPaletteVoronoi uPalette uTime = Effect PaletteVoronoi $ mconcat [upt "palette" uPalette, upf "time" uTime]
pRepeat u = singleUEffect Repeat u
pReverse = Effect Reverse mempty
pRotate u = singleUEffect Rotate u
pScale uX uY = Effect Scale $ mconcat [upf "scale_x" uX, upf "scale_y" uY]
pScale' uXY = pScale uXY uXY
pTranslate uX uY = Effect Translate $ mconcat [upf "translate_x" uX, upf "translate_y" uY]




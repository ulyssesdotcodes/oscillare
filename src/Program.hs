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
  | Edges
  | Fade
  | Filter
  | Fire
  | Lumidots
  | LittlePlanet
  | Mirror
  | Overlay
  | Repeat
  | Reverse
  | Rotate
  | Scale
  | Translate
  deriving Eq

data LayerType =
  Add
  | Displace
  | Mult

data BaseType =
  AbstractSpiral
  | AudioData
  | CircleEmitter
  | Dots
  | Flocking
  | Image
  | InputTexBase
  | Lines
  | Passthrough
  | Shapes
  | SideEmitter
  | Sine
  | StringTheory
  | TextType
  | TriggeredPassthrough
  | Video
  | Voronoi

data Name =
  BaseName BaseType
  | EffName EffectType
  | LayerName LayerType

effectName :: EffectType -> ByteString
effectName Blur = "blur"
effectName Brightness = "brightness"
effectName Edges = "edge_detection"
effectName Fade = "fade"
effectName Filter = "filter"
effectName Fire = "fire"
effectName Lumidots = "lumidots"
effectName LittlePlanet = "little_planet"
effectName Mirror = "mirror"
effectName Overlay = "overlay"
effectName Repeat = "repeat"
effectName Reverse = "reverse"
effectName Rotate = "rotate"
effectName Scale = "scale"
effectName Translate = "translate"

layerName :: LayerType -> ByteString
layerName Add = "add"
layerName Displace = "displace"
layerName Mult = "mult"

baseName :: BaseType -> ByteString
baseName AbstractSpiral = "abstract_spiral"
baseName AudioData = "audio_data"
baseName CircleEmitter = "circle_emitter"
baseName Dots = "dots"
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

baseName Passthrough = "pt"
baseName TriggeredPassthrough = "pt_triggered"

progName :: Name -> ByteString
progName (BaseName b) = baseName b
progName (EffName e) = effectName e
progName (LayerName l) = layerName l

instance Show LayerType where
  show = unpack . layerName

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
  | Layer LayerType [Slot] [Effect]
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
  (Program s (Layer l ss es)) |+| e = Program s $ Layer l ss (e:es)

instance Effectable Exec where
  (Exec ps k es) |+| e = Exec ps k ((-1, [e]):es)

programSlot :: Program -> Slot
programSlot (Program s _) = s

baseProg :: String -> BaseType -> Pattern Uniform -> Program
baseProg s b us = Program (pack s) (SlottableProgram (BaseProgram b us []))

layer :: LayerType -> String -> [String] -> Program
layer l s ss = Program (pack s) (Layer l (pack <$> ss) [])

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
pDots slot uVolume uData = baseProg slot Dots $ (upf "volume" uVolume) `mappend` (upt "eqs" uData)
pCircleEmitter slot uLifespan uVel uRotation uPullback uTex = baseProg slot CircleEmitter $
  mconcat [ upf "delta" deltaPattern
          , upf "time" timePattern
          , upf "emitVel" uVel
          , upf "lifespan" uLifespan
          , upf "pullback" uPullback
          , upf "rotation" uRotation
          , upt "tex_audio" uTex
          ]
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
pFlocking slot uSeparation uMult uSpeed = baseProg slot Flocking $ mconcat [upf "alignment" uMult, upf "cohesion" uMult, upf "separation" uSeparation, upf "time" timePattern, upf "delta" $ deltaPattern, upf "speed" uSpeed]
pLines slot uWidth uSpacing = baseProg slot Lines $ (upf "width" uWidth) `mappend` (upf "spacing" uSpacing)
pImage slot uImage uClear = baseProg slot Image $ mconcat [ups "image" uImage, upf "clear_shade" uClear]
pInput slot uInput = baseProg slot InputTexBase $ upt "tex_input" uInput
pShapes slot uSides uWidth uSize = baseProg slot Shapes $ mconcat [upf "sides" uSides, upf "width" uWidth, upf "size" uSize]
pStringTheory slot uTimeMod uAngle uAngleDelta uXoff = baseProg slot StringTheory $ mconcat [upf "angle" uAngle, upf "angle_delta" uAngleDelta, upf "xoff" uXoff]
pSine slot uXPos uScale uAmplitude = baseProg slot Sine $ mconcat [upf "time" $ uXPos, upf "scale" uScale, upf "amplitude" uAmplitude]
pText slot uText = baseProg slot TextType $ ups "text" uText
pVoronoi slot uTime = baseProg slot Voronoi $ upf "time" uTime
pVideo slot uPath uSpeed = baseProg slot Video $ mconcat [ups "video" uPath, upf "speed" uSpeed]

pt s sp = baseProg s Passthrough (upsWithBase "passthrough" sp)
ptTriggered s sp uTrig = baseProg s TriggeredPassthrough $ upsWithBase "passthroughs" sp `mappend` upf "trigger" uTrig

pBlur = Effect Blur mempty
pBrightness u = singleUEffect Brightness u
pEdges = Effect Edges mempty
pFade u = singleUEffect Fade u
pFilter u = singleUEffect Filter u
pFire u = singleUEffect Fire u
pLittlePlanet = Effect LittlePlanet mempty
pLumidots = Effect Lumidots mempty
pMirror = Effect Mirror mempty
pOverlay u = singleUEffect Overlay u
pRepeat u = singleUEffect Repeat u
pReverse = Effect Reverse mempty
pRotate u = singleUEffect Rotate u
pScale uX uY = Effect Scale $ mconcat [upf "scale_x" uX, upf "scale_y" uY]
pScale' uXY = pScale uXY uXY
pTranslate uX uY = Effect Translate $ mconcat [upf "translate_x" uX, upf "translate_y" uY]

pAdd = layer Add
pDisplace = layer Displace
pMult = layer Mult



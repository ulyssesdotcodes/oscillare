{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Visuals where

import Prelude hiding (floor, mod, lines)

import LambdaDesigner.Op
import LambdaDesigner.ParsedOps
import LambdaDesigner.Lib
import Data.Char
import Data.IORef
import Data.List.Split
import Data.Maybe
import Debug.Trace

import Control.Lens
import Data.Matrix

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L

data VoteType = Movie | Effect deriving Eq
data VoteEffect = VoteEffect VoteType BS.ByteString BS.ByteString BS.ByteString deriving Eq

-- Utility

constnamevalfuncs :: [((Maybe (Tree BS.ByteString) -> Identity (Maybe (Tree BS.ByteString))) -> CHOP -> Identity CHOP, (Maybe (Tree Float) -> Identity (Maybe (Tree Float))) -> CHOP -> Identity CHOP)]
constnamevalfuncs = 
  zip 
    [ constantCHOPname0
    , constantCHOPname1
    , constantCHOPname2
    , constantCHOPname3
    , constantCHOPname4
    , constantCHOPname5
    , constantCHOPname6
    , constantCHOPname7
    , constantCHOPname8
    , constantCHOPname9
    , constantCHOPname10
    , constantCHOPname11
    , constantCHOPname12
    , constantCHOPname13
    , constantCHOPname14
    , constantCHOPname15
    , constantCHOPname16
    , constantCHOPname17
    , constantCHOPname18
    , constantCHOPname19
    ]
    [ constantCHOPvalue0
    , constantCHOPvalue1
    , constantCHOPvalue2
    , constantCHOPvalue3
    , constantCHOPvalue4
    , constantCHOPvalue5
    , constantCHOPvalue6
    , constantCHOPvalue7
    , constantCHOPvalue8
    , constantCHOPvalue9
    , constantCHOPvalue10
    , constantCHOPvalue11
    , constantCHOPvalue12
    , constantCHOPvalue13
    , constantCHOPvalue14
    , constantCHOPvalue15
    , constantCHOPvalue16
    , constantCHOPvalue17
    , constantCHOPvalue18
    , constantCHOPvalue19
    ]
 
constnameval :: Int -> String -> Tree Float -> (CHOP -> CHOP)
constnameval i s f =
    (\(n, v) -> (n ?~ str s) . (v ?~ f)) . flip (!!) i $ constnamevalfuncs

constnamesvals :: [(String, Tree Float)] -> Tree CHOP
constnamesvals namevals = 
  flip constantCHOP [] . foldl (.) id $ 
    zipWith (\(s, f) (n, v) -> (n ?~ str s) . (v ?~ f)) namevals constnamevalfuncs

const1 = constantCHOP (constantCHOPvalue0 ?~ float 1) []

selChans :: String -> Tree CHOP -> Tree CHOP
selChans n c = selectCHOP (selectCHOPchannames ?~ str n) [c]

overChans :: String -> (Tree CHOP -> Tree CHOP) -> Tree CHOP -> Tree CHOP
overChans n f c = replaceCHOP id [c, renameCHOP id . (:[selChans n c]) $ f $ selChans n c]

multchops :: [Tree CHOP] -> Tree CHOP
multchops cs = mathCHOP (mathCHOPchopop ?~ int 3) cs

-- Audio input

ain' m = mathCHOP (mathCHOPgain ?~ float m) [audiodeviceinCHOP id & resampleCHOP ((resampleCHOPtimeslice ?~ bool False) . (resampleCHOPmethod ?~ int 0) . (resampleCHOPrelative ?~ int 0) . (resampleCHOPend ?~ float 0.03)) . (:[])]
ain = ain' 1
atex = choptoTOP (choptoTOPchop ?~ ain)
aspect = audiospectrumCHOP id $ audiodeviceinCHOP id
aspecttex = choptoTOP (choptoTOPchop ?~ aspect)

analyze i = analyzeCHOP (analyzeCHOPfunction ?~ int i)

volume = analyze 6 ain
volc = chan0f volume
lowPass = audiofilterCHOP (audiofilterCHOPfilter ?~ int 0) . (:[])
lowv' = analyze 6 . lowPass . ain'
lowv = lowv' 4
lowvc' = chan0f . lowv'
lowvc = lowvc' 4
highPass = audiofilterCHOP (audiofilterCHOPfilter ?~ int 1) . (:[])
highv = analyze 6 $ highPass ain
highvc = chan0f highv
bandPass b = audiofilterCHOP ((audiofilterCHOPfilter ?~ int 2) . (audiofilterCHOPcutofflog ?~ (b !* float 4.5)))  . (:[])
bandv b = analyze 6 $ bandPass b ain
bandvc = chan0f . bandv

mchan :: String -> Tree Float
mchan s = chanNamef s $ midiinmapCHOP id

mchop :: String -> Tree CHOP
mchop s = midiinmapCHOP id & selectCHOP (selectCHOPchannames ?~ str s) . (:[])

lmf h fing xyz = leapmotionCHOP id & selectCHOP (selectCHOPchannames ?~ str ("hand" ++ show h ++ "/finger" ++ show fing ++ ":t" ++ xyz)) . (:[])
lmf0 h fing = chan0f . lmf h fing
lmp h v = leapmotionCHOP id & selectCHOP (selectCHOPchannames ?~ str ("hand" ++ show h ++ "/palm:" ++ v)) . (:[])
lmp0 h = chan0f . lmp h
lms = leapmotionCHOP id & selectCHOP (selectCHOPchannames ?~ str "swipe0:tracking") . (:[])
lms0 = chan0f $ lms

cTSMod tf s = choptoSOP (
                (choptoSOPattscope ?~ str "P") .
                (choptoSOPchop ?~ (tf (soptoCHOP (soptoCHOPsop ?~ s))))
                )

amult t = mathCHOP (
            (mathCHOPalign ?~ int 7) . 
            (mathCHOPchopop ?~ int 3)
            ) [t, mathCHOP ((mathCHOPpostoff ?~ (float 1)) . (mathCHOPgain ?~ float 2)) [ain]]

acirc = cTSMod amult (circleSOP ((circleSOParc ?~ int 1) . (circleSOPtype ?~ int 2)) [])

asphere = cTSMod amult $ sphereSOP (sphereSOPtype ?~ int 3) []

mnoise t s = noiseCHOP ((noiseCHOPtype ?~ int 2) .
                        (noiseCHOPperiod ?~ float s) .
                        (noiseCHOPt . _2 ?~ t) .
                        (noiseCHOPtimeslice ?~ bool True)) []
mnoisec t s = chan0f $ mnoise t s

-- launchmapping = strobe (float 10 !* mchan "s1c")
--   $ scalexy (float 0.2 !* mchan "s1b")
--   $ fade (mchan "s1")
--   $ foldr (.) id (zipWith ($) (reverse effects) (reverse [1..(length effects)]))
--   $ switchT (float (-1) !+ (chan0f $ hold buttons buttons))
--           [adata (mchan "s1a" !* float 2), shapes (float 3 !+ scycle 1 3) (volc !* mchan "s2a") (mchan "s2b")]

-- effects = [ \n -> palettecycle' (passmchan n) neon seconds
--           , \n -> translatex' (passmchan n) (mchan ("s" ++ show n) !* seconds)
--           , \n -> paletterepeatT' (passmchan n) neon (float 20 !* mchan ("s" ++ show n))
--           , \n -> mirror' (passmchan n)
--           , \n -> mosaic' (passmchan n) (seconds !* float 20) (float 60)
--           , \n -> blur' (passmchan n) (float 56 !* mchan ("s" ++ show n))
--           ]
-- passmchan m = topPasses ?~ casti (mchan $ "b" ++ show m)

-- buttons = mathCHOP (mathCombChops ?~ int 1) $ mheld <$> [1..4]
-- mheld n = constC [float (fromIntegral n) !* (mchan $ "b" ++ show (n + 8))]

-- stresstest = fadeops (float 0.5) [
--        noisedisplace (float 10) $ mosaic (seconds !* float 20) (float 100) $
--        fade (float 0.96) $ blur (float 128) $ palettecycle neon seconds $
--        flocking (float 0.5, float 1) (float 10 !* volc),
--        blur (float 27) $
--        fade (float 0.99) $ flocking (float 0.4, float 1) (float 10 !* volc)]

-- -------------------

-- sphereNoise = geo' id $ outS asphere

-- -- Gens
adata m = tdata m atex
flocking (c, s) sp = tox0 "toxes/Visuals/flockingGpu.tox" [ ("Cohesion", ResolveP c)
                                                            , ("Separation", ResolveP s)
                                                            , ("Alignment", ResolveP c)
                                                            , ("Speed", ResolveP sp)
                                                            ]
lines w s = frag "lines.frag" [("i_width", xV4 w), ("i_spacing", xV4 s)] []
-- metaballs mat = let wrapJust n x = Just $ chan0f x !* float n
--                     mball n r tx ty = metaball' ((metaballRadius .~ (wrapJust n r, wrapJust n r, wrapJust n r)) .
--                                                 (metaballCenter .~ (Just tx, Just ty, Nothing)))
--                     noiset m = noiseC' ((chopTimeSlice ?~ bool True) .
--                                         (noiseCTranslate._1 ?~ seconds !* float 0.3) .
--                                         (noiseCTranslate._3 ?~ float (m * 3)) .
--                                         (noiseCAmplitude ?~ (float (m + 1)) !* volc) .
--                                         (noiseCChannels ?~ str "chan[1-3]"))
--                     noisex = noiset 1
--                     noisey = noiset 0
--                     lagmodC = lag (float 0) (float 0.2)
--                 in rendered . geo' (geoMat ?~ mat) .
--                    outS $ mergeS [ mball 1 (lagmodC lowv) (chanf 0 noisex !+ float 0.2)
--                                    (chan0f noisey)
--                                  , mball 9 (lagmodC highv)
--                                    (chanf 1 noisex !+ float (-0.2))
--                                    (chanf 1 noisey !+ float 0.7)
--                                  , mball 4 (lagmodC $ bandv (float 0.5))
--                                    (chanf 2 noisex !+ float (-0.2))
--                                    (chanf 2 noisey !+ float (-0.7))
--                                  ]
-- movingSquiggly = geo' ((geoTranslate .~ (Just $ mnoisec (seconds !* float 20) 5, Just $ mnoisec (seconds !* float 20) 10, Just $ float  0)) .
--             (geoScale.each ?~ float 0.3) .
--             (geoMat ?~ constM' (constColor .~ (Just $ osin $ seconds, Just $ osin $ (seconds !* float 2), Just $ osin $ (seconds !* chan0f volume)))))
--             $ outS acirc

particlemover v a p s f = tox0 "toxes/Visuals/particlemover.tox" [ ("Palette", ResolveP $ palette p)
                                                              , ("Vmult", ResolveP v)
                                                              , ("Emitalpha", ResolveP a)
                                                              , ("Force", ResolveP f)
                                                              , ("Shape", ResolveP s)
                                                              ]

shapes sides w s = frag "shapes.frag" [ ("i_size", xV4 s)
                                      , ("i_width", xV4 w)
                                      , ("i_sides", xV4 sides)
                                      ] []

sineT x s a = frag "sine.frag" [("i_time", xV4 $ x), ("i_scale", xV4 $ s), ("i_amplitude", xV4 $ a)] []
stringtheory t a = frag "string_theory.frag" [ ("i_time", xV4 t)
                                             , ("i_angle", xV4 a)
                                             , ("i_angle_delta", xV4 $ float 0.2)
                                             , ("i_xoff", xV4 $ float 0)
                                             ] []
-- movie s f = movieFileIn' ((moviePlayMode ?~ int 1) .
--                           (movieIndex ?~ casti s) .
--                           (topResolution .~ iv2 (1920, 1080))) $ str $ "videos/" ++ f

-- geoT tr sc top sop = render [geo' ((geoTranslate .~ tr) . (geoScale .~ sc) . (geoMat ?~ topM top)) (outS sop)] cam
-- commandCode t = textT' ( (topResolution .~ (Just $ int 1920, Just $ int 1080))
--                          . (textFontSize ?~ float 16)
--                          . (textAlign .~ iv2 (0, 0))
--                          ) (str t)

-- -- Geometry generators and utilities

-- sinC i = waveC' (waveCNames ?~ str "rz") i $ osin (castf sampleIndex) !* float 360
-- scaleC i n = waveC' (waveCNames ?~ str "sx") i $ castf sampleIndex !* n

-- sidesTorus sides scale = torus' ((torusOrientation ?~ int 2) . (torusRows ?~ int 10) . (torusColumns ?~ sides) . (torusRadius .~ v2 (scale !* float 1) (scale !* float 0.5)))

-- lineLines width scale inst sop =
--   let
--     instances = casti inst !+ int 2
--   in
--     lineGeo (mathCHOP (mathCHOPgain ?~ scale) [ain]) (sinC instances) (scaleC instances $ float 0.1) (scaleC instances $ float 0.1) sop width instances wireframeM

-- lineGeo ty rz sx sy sop width instances mat =
--   let
--     ain = mathCHOP (mathCHOPgain ?~ float 10) [audioIn]
--     sgeo = instanceGeo' ((geoMat ?~ mat)) poses (outS $ sop)
--     poses = mergeC' (mergeCAlign ?~ int 7) [tx, ty', rz & renameC (str "rz"), sx & renameC (str "sx"), sy & renameC (str "sy")]
--     tx = waveC' (waveCNames ?~ str "tx") instances $ ((castf (sampleIndex !* casti width !* int 2)) !/ castf instances) !- width
--     ty' = ty & resampleC' ((resampleEnd ?~ instances) . (resampleRate ?~ instances)) False & renameC (str "ty")
--     centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
--     volume = analyze (int 6) ain
--     volc = chan0f volume
--   in
--     render [sgeo] (centerCam (v3 (float 0) (float 0) (float 50)) emptyV3)

-- spiralGeo inst speed sop =
--   let
--     sgeo = instanceGeo' ((geoMat ?~ wireframeM) . (geoUniformScale ?~ float 0.1)) poses (outS $ sop)
--     instances = casti $ inst
--     poses = mergeC' (mergeCAlign ?~ int 7) [ty, tx, tz]
--     instanceIter n = (castf sampleIndex !+ (speed !* float n) !% castf instances)
--     tx = waveC' (waveCNames ?~ str "tx") instances $ ocos (castf sampleIndex !* float 60 !+ instanceIter 0.2) !* ((instanceIter 10 !* float 0.1) !+ float 4)
--     ty = waveC' (waveCNames ?~ str "ty") instances $ osin (castf sampleIndex !* float 60 !+ instanceIter 0.2) !* ((instanceIter 10 !* float 0.1) !+ float 4)
--     tz = waveC' (waveCNames ?~ str "tz") instances $ instanceIter 10 !* float 1 !- float 50
--     centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
--   in
--     render [sgeo] (centerCam (v3 (float 0) (float 0) (float 5)) emptyV3)

-- -- vidIn

-- -- Effects

xV2 x = (Just x, Nothing)
xV3 x = (Just x, Nothing, Nothing)
xV4 x = (Just x, Nothing, Nothing, Nothing)

fade' f l o t = feedbackT t (\t' -> l $ compositeTOP (compositeTOPoperand ?~ int 0) [t, levelTOP (levelTOPopacity ?~ o) t']) f
fade = fade' id id
over' f l o t = feedbackT t (\t' -> l $ compositeTOP (compositeTOPoperand ?~ int 31) [t, levelTOP (levelTOPopacity ?~ o) t']) f
over = over' id id
brightness' f b = levelTOP (levelTOPbrightness1 ?~ b)
brightness = brightness' id
--blur
crosshatch' f = frag' f "crosshatch.frag" [] . (:[])
paletterepeatT' f p r top = frag' f "color_repeat.frag" [("i_repeat", xV4 r)] [top, palette p]
paletterepeatT = paletterepeatT' id
edgesc' f c t = compositeTOP (compositeTOPoperand ?~ int 0) [edgeTOP f t, levelTOP (levelTOPopacity ?~ c) t]
edgesc = edgesc' id
flowermod s = frag' id "flower_mod.frag" [("uSeconds", xV4 s)] . (:[])
hue h = hsvadjustTOP (hsvadjustTOPhueoffset ?~ h)
littleplanet' f = frag' f "little_planet.frag" [] . (:[])
littleplanet = littleplanet' id
lumidots' f = frag' f "lumidots.frag" [] . (:[])
lumidots = lumidots' id
mirror' f t = compositeTOP ((compositeTOPoperand ?~ int 0) . f) [flipTOP ((flipTOPflipx ?~ bool True) . (flipTOPflipy ?~ bool True)) t, t]
mirror = mirror' id
mosaic' f t s top = frag' f "mosaic.frag" [("uTime", xV4 t), ("uScale", xV4 s)] [top]
mosaic = mosaic' id
noisedisplace' f d top = frag' f "noise_displace.frag" [("uTime", xV4 seconds), ("uDisplacement", xV4 d)] [top]
noisedisplace = noisedisplace' id
palettecycle' f p@(Palette _ s) t = compositeTOP ((compositeTOPoperand ?~ int 27) . (compositeTOPformat ?~ int 4) . f) [cropTOP ((cropTOPcropleft ?~ s) . (cropTOPcropright ?~ s)) $ palette p, t]
palettecycle = palettecycle' id
palettemap' f p@(Palette _ o) t = frag' f "palette_map.frag" [("uOffset", xV4 o), ("uSamples", xV4 $ float 16)] [t, palette p]
palettemap = palettemap' id
repeatT' f s = transformscale' f s 2
repeatT = repeatT' id
repeatTxy' f r = repeatT' f (Just r, Just r)
repeatTxy = repeatTxy' id
rgbsplit' s top = frag' id "rgbsplit.frag" [("uFrames", xV4 s)] [top]
rotate' f r = transformTOP (transformTOProtate ?~ r)
rotate = rotate' id
sat s = hsvadjustTOP (hsvadjustTOPsaturationmult ?~ s)
scale' f s = transformscale' f s 1
scale = scale' id
scalexy' f s = scale' f (Just s, Just s)
scalexy = scalexy' id
strobe' f s top = frag' f "strobe.frag" [("uSpeed", xV4 s), ("uTime", xV4 seconds)] [top]
strobe = strobe' id
transformext' f e = transformTOP (f . (transformTOPextend ?~ (int e)))
transformscale' f s e = transformext' (f . (transformTOPs .~ ((!^ (float (-1))) <$> fst s, (!^ (float (-1))) <$> snd s) )) e
transformscale = transformscale' id
translate' f t = transformTOP (f . (transformTOPextend ?~ int 3) . (transformTOPt .~ t))
translate (a, b) = translate' id (Just a, Just b)
translatex' f x = translate' f $ (Just x, Just $ float 0)
translatex = translatex' id
translatey' f y = translate' f $ (Just $ float 0, Just y)
translatey = translatey' id
val v = hsvadjustTOP (hsvadjustTOPvaluemult ?~ v)

clone c (tx, ty) (sx, sy) top = frag' id "clone.frag" [("uClones", xV4 c), ("uTranslate", emptyV4 & _1 ?~ tx & _2 ?~ ty), ("uScale", emptyV4 & _1 ?~ sx & _2 ?~ sy)] [top]

-- -- combiners

addops = compositeTOP (compositeTOPoperand ?~ int 0)
fadeops f = switchTOP ((switchTOPblend ?~ bool True) . (switchTOPindex ?~ f))
multops = compositeTOP (compositeTOPoperand ?~ int 27)
overops = compositeTOP (compositeTOPoperand ?~ int 31)
triggercount f l = countCHOP ((countCHOPthreshold ?~ bool True) .
                              (countCHOPthreshup ?~ float 0.5) .
                              (countCHOPlimitmax ?~ float (fromIntegral $ l)) .
                              (countCHOPoutput ?~ int 1)
                            ) f
triggerops f tops = switchTOP (switchTOPindex ?~ chan0f (triggercount f (length tops - 1))) tops
-- showwork g es =
--   let
--     scans = scanl (\g e -> e g) g es
--   in
--     addops $ (head $ reverse scans):(zipWith (\i -> (translate (float (-0.45) !+ (float 0.1 !* float i), float 0.4)) . scalexy (float 10)) [0..] (take (length es) $ scans))

-- -- led
-- (>>>) = flip (.)
-- constled f n = constC' (constCEndFrames ?~ int n) [f]
-- rgbled n r g b = mergeC [r & stretchC (int n), g & stretchC (int n), b & stretchC (int n)]
--                & shuffleC (int 6) 
-- topToLed n = 
--     crop' ((cropTop ?~ float 0.00008))
--     >>> topToC
--     >>> selectC' (selectCNames ?~ str "r g b")
--     >>> stretchC (int n)
--     >>> shuffleC (int 6)
-- bottomLedSplit n t = (topToLed n t, t)
-- ledPalette p = flip lookupC (topToC' (topToChopAName ?~ str "") $ palette (p. (switchTOPindex ?~ )))

-- palettes

data Palette = Palette [Color] (Tree Float)
data Color = Hex BS.ByteString | RGB Int Int Int
neon = Palette $ Hex <$> ["A9336B", "5F2F88", "CB673D", "87BB38"]
fire = Palette $ Hex . BS.pack . fmap toUpper <$> ["F07F13", "800909", "F27D0C", "FDCF58"]
buddhist = Palette $ Hex . BS.pack . fmap toUpper <$> ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealcontrast = Palette [RGB 188 242 246, RGB 50 107 113, RGB 211 90 30, RGB 209 122 43, RGB 188 242 246]
purplish = Palette [RGB 150 110 100, RGB 223 143 67, RGB 76 73 100 , RGB 146 118 133, RGB 165 148 180]
sunset = Palette [RGB 185 117 19, RGB 228 187 108, RGB 251 162 1, RGB 255 243 201]
coolpink = Palette [RGB 215 40 26, RGB 157 60 121, RGB 179 83 154, RGB 187 59 98]
darkestred = Palette [RGB 153 7 17, RGB 97 6 11, RGB 49 7 8, RGB 13 7 7, RGB 189 5 13]
nature = Palette [RGB 63 124 7, RGB 201 121 66, RGB 213 101 23, RGB 177 201 80, RGB 180 207 127]
greenpurple = Palette [RGB 42 4 74, RGB 11 46 89, RGB 13 103 89, RGB 122 179 23, RGB 160 197 95]
tealblue = Palette [RGB 188 242 246, RGB 50 107 113, RGB 188 242 246, RGB 165 148 180]

palettergb :: Palette -> Tree CHOP
palettergb (Palette colors sel) = 
  crossCHOP (crossCHOPcross ?~ (sel !% float 1) !* (float $ fromIntegral $ length colors)) $ constnamesvals . zip ["r", "g", "b"] . (fmap float) . colorToFloat <$> (last colors):colors

palettergba :: Palette -> Tree CHOP
palettergba (Palette colors sel) = 
  crossCHOP (crossCHOPcross ?~ (sel !% float 1) !* (float $ fromIntegral $ length colors)) $ constnamesvals . zip ["r", "g", "b", "a"] . (fmap float) . colorToFloat <$> (last colors):colors

paletteMapSel :: (Tree Float -> Tree Float) -> Palette -> Palette
paletteMapSel f (Palette p s) = Palette p (f s)

paletteClampColor :: Palette -> Palette
paletteClampColor (Palette cs s) = Palette cs (floor s !/ float (fromIntegral $ length cs))

-- ------------------------

-- lagmod l = chan0f . lag (float 0) (float l) . constC . (:[])
scr = (++) "scripts/Visuals/"
frag = frag' id
frag' f s params = 
  glslmultiTOP (
    f . (glslmultiTOPresolutionw ?~ int 1920) . 
    (glslmultiTOPresolutionh ?~ int 1080) . 
    (glslmultiTOPpixeldat ?~ textDAT (textDATfile ?~ str (scr s)) []) . 
    (glslmultiTOPoutputresolution ?~ int 9) .
    (glslmultiTOPformat ?~ int 4) .
    (foldl (.) id $ zipWith3 (\(pn, pv) n v -> (n ?~ str pn) . (v .~ pv)) params 
      [glslmultiTOPuniname0, glslmultiTOPuniname1, glslmultiTOPuniname2, glslmultiTOPuniname3] 
      [glslmultiTOPvalue0, glslmultiTOPvalue1, glslmultiTOPvalue2, glslmultiTOPvalue3]))
-- rendered g = renderTOP ((renderTOPlights .~ [lightCOMP id []]) . (renderTOPgeometry ?~ ResolveP g) . (renderTOPcamera ?~ ResolveP (cameraCOMP id [])))
tdata v t = frag "audio_data.frag" [("i_volume", xV4 v)] [t]
palette p = rampTOP ((rampTOPresolutionw ?~ int 128) . (rampTOPresolutionh ?~ int 1) . (rampTOPdat ?~ paletteDat p)) []
paletteDat (Palette colors sel) = tableDAT (textBlob ?~ BS.intercalate "\n" (BS.intercalate "\t" <$> ["r", "g", "b", "a", "pos"]:(zipWith (colorToBS (length colors)) [0..] colors)))

colorToBS :: Int -> Int -> Color -> [BS.ByteString]
colorToBS n i c = ((BS.pack . show) <$> colorToFloat c)  ++ [BS.pack . show $ fromIntegral i / fromIntegral n]

colorToFloat :: Color -> [Float]
colorToFloat (Hex str) =
  let
    hexes = take 3 . chunksOf 2 . drop 1
    todig = flip L.elemIndex "0123456789ABCDEF"
    toIntList = fmap todig
    toInt = foldl (\acc i -> acc * 16 + i) 0
    toHex = fmap toInt . sequence . toIntList
    hextorgb = fmap ((/ 256) . fromIntegral)
  in
    catMaybes $ (hextorgb <$> (toHex <$> hexes (show str))) ++ [Just 1]
colorToFloat (RGB r g b) = fmap ((/ 256) . fromIntegral) [r, g, b, 256]



-- ------------------------------
-- -- Randos

-- tidalmessages = oscinD' (oscInDAddressScope ?~ str "/vis") 7010 & selectD' ((selectDRStartI ?~ int 1) . (selectDREndI ?~ int 1) . (selectDCStartI ?~ int 5) . (selectDCEndI ?~ int 5))
-- tidalvis = tox "toxes/tidal_patterns.tox" [] (Just tidalmessages)
-- tidalvistop = tidalvis & nullC & chopToT' (chopToTopFormat ?~ int 2)


-- sensel

sensel = cplusplusCHOP ( (cplusplusCHOPplugin ?~ str "C:/Users/ulyssesp/Development/SenselCHOP/Release/CPlusPlusCHOPExample.dll")) []
senselchop = sensel & selectCHOP (selectCHOPchannames ?~ str "chan") . (:[]) & shuffleCHOP ((shuffleCHOPmethod ?~ int 8) . (shuffleCHOPnval ?~ int 185)) 
senseltop f = 
  choptoTOP (choptoTOPchop ?~ f senselchop) 
  & flipTOP (flipTOPflipy ?~ bool True) 
  & resolutionTOP ((resolutionTOPresolutionw ?~ int 1920) . (resolutionTOPresolutionh ?~ int 1080) . (resolutionTOPoutputresolution ?~ int 9)) 
  & reorderTOP ((reorderTOPformat ?~ int 26) . (reorderTOPoutputalphachan ?~ int 0)) . (:[])
senseltouches = sensel & selectCHOP (selectCHOPchannames ?~ str "chan1") . (:[]) & deleteCHOP ((deleteCHOPdelsamples ?~ bool True)) . (:[])

-- beats
data Beat = 
    Beat 
        { beatpulse :: Tree CHOP
        , bps :: Tree CHOP
        }

tapbeat :: Tree Float -> Tree Float -> Beat
tapbeat i g = 
  let
    beat = constantCHOP (constantCHOPvalue0 ?~ i) [] & logicCHOP (logicCHOPpreop ?~ int 5) . (:[])
    beathold = holdCHOP id [speedCHOP id [const1, beat] & delayCHOP ((delayCHOPdelay ?~ float 1) . (delayCHOPdelayunit ?~ int 1)), beat] & nullCHOP (nullCHOPcooktype ?~ int 2)
    beattrail = 
        trailCHOP ((trailCHOPwlength ?~ float 8) . (trailCHOPwlengthunit ?~ int 1) . (trailCHOPcapture ?~ int 1)) [beathold]
        & deleteCHOP ((deleteCHOPdelsamples ?~ bool True) . (deleteCHOPcondition ?~ int 5) . (deleteCHOPinclvalue1 ?~ bool False)) . (:[])
    bps = mathCHOP ((mathCHOPpostop ?~ int 5) . (mathCHOPgain ?~ g)) . (:[]) $ analyze 0 beattrail
    beataccum = speedCHOP id [bps, beat]
    finalbeat = 
        beataccum 
            & limitCHOP ((limitCHOPmax ?~ float 1) . (limitCHOPtype ?~ int 2) . (limitCHOPmin ?~ float 0)) 
            & logicCHOP ((logicCHOPboundmax ?~ float 0.08) . (logicCHOPpreop ?~ int 5) . (logicCHOPconvert ?~ int 2)) . (:[])
  in
    Beat finalbeat bps

tapbeatm9 = tapbeat (mchan "b9") (float 2 !^ (floor $ (mchan "s1a" !- float 0.5) !* float 4))
tapbeatm9sec = beatseconds tapbeatm9

beatramp :: Beat -> Tree CHOP
beatramp (Beat beat bps) = speedCHOP (speedCHOPresetcondition ?~ int 2) [bps, beat]

beatxcount :: Float -> Tree CHOP -> Beat -> Tree CHOP
beatxcount x reset (Beat beat _) = countCHOP ((countCHOPoutput ?~ int 1) . (countCHOPlimitmax ?~ float (x - 1))) [beat, reset]

beatxpulse :: Float -> Tree CHOP -> Beat -> Tree CHOP
beatxpulse x reset = logicCHOP (logicCHOPpreop ?~ int 6) . (:[]) . beatxcount x reset

beatxramp :: Float -> Tree CHOP -> Beat -> Tree CHOP
beatxramp x reset beat@(Beat bpulse bps) = speedCHOP id [bps & mathCHOP (mathCHOPgain ?~ float (1/x)) . (:[]), beatxpulse x reset beat]

beatseconds :: Beat -> Tree Float
beatseconds b = seconds !* (chan0f $ bps b)

beatsecondschop :: Beat -> Tree CHOP
beatsecondschop b = speedCHOP id [bps b]


-- dj midi clock

midi = midiinCHOP (
        (midiinCHOPid ?~ str "2") . 
        (midiinCHOPsimplified ?~ bool False) . 
        (midiinCHOPpulsename ?~ str "beat") .
        (midiinCHOPchannel ?~ str "1-8") .
        (midiinCHOPcontrolind ?~ str "1-16")
        )
midisyncbeat = tapbeat (chan0f $ midi & selectCHOP (selectCHOPchannames ?~ str "beat") . (:[]))

-- DMX

data DMXColor = DMXColor (Tree CHOP)
data DMX = 
    COLORado (Tree CHOP) DMXColor
    | GenericRGB DMXColor
    | Laluce (Tree CHOP) DMXColor (Tree CHOP)


dmxToChop :: DMX -> Tree CHOP
dmxToChop (COLORado dim (DMXColor color)) = 
    mergeCHOP id 
        [ dim
        , color
        , constnamesvals $ (, float 0) <$> ["color_macro", "strobe", "progs", "auto_speed", "dimmer_speed"] 
        ]
dmxToChop (GenericRGB (DMXColor tch)) = tch
dmxToChop (Laluce dim (DMXColor color) white) = 
    mergeCHOP id 
        [ dim
        , color
        , white
        , constnamesvals $ (, float 0) <$> ["hopping", "strobe", "macro"] 
        ]

dmxColor = DMXColor $ constnamesvals [("r", float 0), ("g", float 0), ("b", float 0)]
laluce = Laluce (const1) dmxColor (constnamesvals [("w", float 0)]) 

dmxColorRGB :: Tree Float -> Tree Float -> Tree Float -> DMXColor
dmxColorRGB r g b = DMXColor $ constnamesvals [("r", r), ("g", g), ("b", b)]

dimDmx :: Tree CHOP -> DMX -> DMX
dimDmx f (COLORado dim c) = COLORado (multchops [dim, f]) c
dimDmx f (Laluce dim c w) = Laluce (multchops [dim, f]) c w
dimDmx f (GenericRGB (DMXColor tch)) = GenericRGB (DMXColor $ multchops [tch, f])

dmxMapColor :: (DMXColor -> DMXColor) -> DMX -> DMX
dmxMapColor f (COLORado dim dmxc) = COLORado dim (f dmxc)
dmxMapColor f (GenericRGB dmxc) = GenericRGB (f dmxc)
dmxMapColor f (Laluce dim dmxc w) = Laluce dim (f dmxc) w

dmxColorMapR :: (Tree CHOP -> Tree CHOP) -> DMXColor -> DMXColor
dmxColorMapR f (DMXColor c) = DMXColor $ overChans "r" f c

dmxColorMapG :: (Tree CHOP -> Tree CHOP) -> DMXColor -> DMXColor
dmxColorMapG f (DMXColor c) = DMXColor $ overChans "g" f c

dmxColorMapB :: (Tree CHOP -> Tree CHOP) -> DMXColor -> DMXColor
dmxColorMapB f (DMXColor c) = DMXColor $ overChans "b" f c

dmxColorMapAll :: (Tree CHOP -> Tree CHOP) -> DMXColor -> DMXColor
dmxColorMapAll f (DMXColor c) = DMXColor $ overChans "r g b" f c

homeDmx = [laluce]

sendDmx :: [DMX] -> Tree CHOP
sendDmx = dmxoutCHOP (
  (dmxoutCHOPinterface ?~ int 3) . 
  (dmxoutCHOPrate ?~ int 40) . 
  (dmxoutCHOPnetaddress ?~ str "10.7.224.159") . 
  (dmxoutCHOPlocaladdress ?~ str "10.7.224.158")) . mathCHOP (mathCHOPtorange2 ?~ float 255) . (:[]) . mergeCHOP id . fmap dmxToChop

paletteDmx :: Palette -> DMX -> DMX
paletteDmx p d = dmxMapColor (dmxColorMapAll (const $ palettergb p)) d

paletteDmxList :: Palette -> [DMX] -> [DMX]
paletteDmxList p@(Palette _ f) ds = zipWith (\i -> paletteDmx (paletteMapSel ((!+) (float $ fromIntegral i / fromIntegral (length ds))) p)) [0..] ds
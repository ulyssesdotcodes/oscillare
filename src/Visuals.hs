{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Visuals where

import Prelude hiding (floor, mod, lines)
import OSCServer

import LambdaDesigner.Op
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

compRunner :: IO ((Tree CHOP, Tree TOP) -> IO ())
compRunner = do init <- newIORef mempty
                return $ \(a, b) -> run2 init [outT $ b] [outC $ a]

ledRunner :: IO ((Tree CHOP, Tree TOP) -> IO ())
ledRunner = 
  let
    leddata a = a & shuffleC (int 6) & limitC (int 1) (float 0) (float 1)
    sendleddata a = fileD' (datVars .~ [("leddata", Resolve $ leddata a), ("arduino", Resolve $ arduino "COM10" 6)]) "scripts/sendleddata.py"                                   
    execArduino a = executeD' ((executeDatFrameend ?~ "mod(me.fetch(\"sendleddata\")[1:]).onFrameUpdate(frame)") . (datVars .~ [("sendleddata", Resolve $ sendleddata a)])) []
  in do init <- newIORef mempty
        return $ \(a, b) -> run2 init [execArduino a] [outT $ b]

ain' m = math' (mathMult ?~ float m) [audioIn]
ain = ain' 1
atex = chopToT $ ain
aspect = audioSpectrum $ audioIn
aspecttex = chopToT $ aspect

volume = analyze (int 6) ain
volc = chan0f volume
lowv' = analyze (int 6) . lowPass . ain'
lowv = lowv' 4
lowvc' = chan0f . lowv'
lowvc = lowvc' 4
highv = analyze (int 6) $ highPass ain
highvc = chan0f highv
bandv b = analyze (int 6) $ bandPass b ain
bandvc = chan0f . bandv

lmf h fing xyz = leapmotion & selectC' (selectCNames ?~ str ("hand" ++ show h ++ "/finger" ++ show fing ++ ":t" ++ xyz))
lmf0 h fing = chan0f . lmf h fing
lmp h v = leapmotion & selectC' (selectCNames ?~ str ("hand" ++ show h ++ "/palm:" ++ v))
lmp0 h = chan0f . lmp h
lms = leapmotion & selectC' (selectCNames ?~ str "swipe0:tracking")
lms0 = chan0f $ lms

cTSMod tf s = chopToS' ((chopToSResample ?~ bool True) . (chopToSopAttrScope ?~ str "P")) (tf (sopToC s)) (Just s)

amult t = math' ((mathAlign ?~ int 7) . (mathCombChops ?~ int 3)) [t, math' ((mathAddPost ?~ (float 1)) . (mathMult ?~ float 2)) [ain]]

acirc = cTSMod amult (circleS' ((circArc ?~ int 1) . (circType ?~ int 2)))

asphere = cTSMod amult $ sphere' (sphereType ?~ int 3)

mnoise t s = noiseC' ((noiseCType ?~ int 2) .
                         (noiseCPeriod ?~ float s) .
                         (noiseCTranslate._2 ?~ t) .
                         (chopTimeSlice ?~ bool True))
mnoisec t s = chan0f $ mnoise t s

tapbeat i = 
  let
    beat = constC [i] & logic' (logicPreop ?~ int 5) . (:[])
    beatdelay = beat & delay (int 1)
    beatspeed = expressionC [ternary (chan0f (opInput (int 0)) !> float 2) (float 0) (chan0f (opInput (int 0)))] 
                  [speedC (beatdelay & logic' (logicPreop ?~ int 1) . (:[])) (Just beatdelay)]
    beathold = hold beatspeed beat
    beattrail = trailC' ((trailActive ?~ chan0f beat !== float 1) . (trailWindowLengthFrames ?~ int 10) . (trailCapture ?~ int 1)) beathold
    beataccum = speedC (math' (mathPostOp ?~ int 5) . (:[]) $ analyze (int 0) beattrail) (Just beat)
  in
    expressionC [castf $ ((chan0f $ opInput (int 0)) !% float 1 !> float 0) !&& ((chan0f $ opInput (int 0)) !% float 1 !< float 0.16)] [beataccum]

launchmapping = strobe (float 10 !* mchan "s1c")
  $ scalexy (float 0.2 !* mchan "s1b")
  $ fade (mchan "s1")
  $ foldr (.) id (zipWith ($) (reverse effects) (reverse [1..(length effects)]))
  $ switchT (float (-1) !+ (chan0f $ hold buttons buttons))
          [adata (mchan "s1a" !* float 2), shapes (float 3 !+ scycle 1 3) (volc !* mchan "s2a") (mchan "s2b")]

effects = [ \n -> palettecycle' (passmchan n) neon seconds
          , \n -> translatex' (passmchan n) (mchan ("s" ++ show n) !* seconds)
          , \n -> paletterepeatT' (passmchan n) neon (float 20 !* mchan ("s" ++ show n))
          , \n -> mirror' (passmchan n)
          , \n -> mosaic' (passmchan n) (seconds !* float 20) (float 60)
          , \n -> blur' (passmchan n) (float 56 !* mchan ("s" ++ show n))
          ]
passmchan m = topPasses ?~ casti (mchan $ "b" ++ show m)

buttons = math' (mathCombChops ?~ int 1) $ mheld <$> [1..4]
mheld n = constC [float (fromIntegral n) !* (mchan $ "b" ++ show (n + 8))]

stresstest = fadeops (float 0.5) [
       noisedisplace (float 10) $ mosaic (seconds !* float 20) (float 100) $
       fade (float 0.96) $ blur (float 128) $ palettecycle neon seconds $
       flocking (float 0.5, float 1) (float 10 !* volc),
       blur (float 27) $
       fade (float 0.99) $ flocking (float 0.4, float 1) (float 10 !* volc)]

-------------------

sphereNoise = geo' id $ outS asphere

-- Gens
adata m = tdata m atex
flocking (c, s) sp = tox "toxes/Visuals/flockingGpu.tox" [ ("Cohesion", ResolveP c)
                                                            , ("Separation", ResolveP s)
                                                            , ("Alignment", ResolveP c)
                                                            , ("Speed", ResolveP sp)
                                                            ] (Nothing :: Maybe (Tree TOP))
lines w s = frag "lines.frag" [("i_width", xV4 w), ("i_spacing", xV4 s)] []
metaballs mat = let wrapJust n x = Just $ chan0f x !* float n
                    mball n r tx ty = metaball' ((metaballRadius .~ (wrapJust n r, wrapJust n r, wrapJust n r)) .
                                                (metaballCenter .~ (Just tx, Just ty, Nothing)))
                    noiset m = noiseC' ((chopTimeSlice ?~ bool True) .
                                        (noiseCTranslate._1 ?~ seconds !* float 0.3) .
                                        (noiseCTranslate._3 ?~ float (m * 3)) .
                                        (noiseCAmplitude ?~ (float (m + 1)) !* volc) .
                                        (noiseCChannels ?~ str "chan[1-3]"))
                    noisex = noiset 1
                    noisey = noiset 0
                    lagmodC = lag (float 0) (float 0.2)
                in rendered . geo' (geoMat ?~ mat) .
                   outS $ mergeS [ mball 1 (lagmodC lowv) (chanf 0 noisex !+ float 0.2)
                                   (chan0f noisey)
                                 , mball 9 (lagmodC highv)
                                   (chanf 1 noisex !+ float (-0.2))
                                   (chanf 1 noisey !+ float 0.7)
                                 , mball 4 (lagmodC $ bandv (float 0.5))
                                   (chanf 2 noisex !+ float (-0.2))
                                   (chanf 2 noisey !+ float (-0.7))
                                 ]
movingSquiggly = geo' ((geoTranslate .~ (Just $ mnoisec (seconds !* float 20) 5, Just $ mnoisec (seconds !* float 20) 10, Just $ float  0)) .
            (geoScale.each ?~ float 0.3) .
            (geoMat ?~ constM' (constColor .~ (Just $ osin $ seconds, Just $ osin $ (seconds !* float 2), Just $ osin $ (seconds !* chan0f volume)))))
            $ outS acirc

particlemover v a p s = tox "toxes/Visuals/particlemover.tox" [ ("Palette", ResolveP $ palette p)
                                                              , ("Vmult", ResolveP v)
                                                              , ("Emitalpha", ResolveP a)
                                                              , ("Shape", ResolveP s)
                                                              ] (Nothing :: Maybe (Tree TOP))

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
movie s f = movieFileIn' ((moviePlayMode ?~ int 1) .
                          (movieIndex ?~ casti s) .
                          (topResolution .~ iv2 (1920, 1080))) $ str $ "videos/" ++ f

geoT tr sc top sop = render [geo' ((geoTranslate .~ tr) . (geoScale .~ sc) . (geoMat ?~ topM top)) (outS sop)] cam
commandCode t = textT' ( (topResolution .~ (Just $ int 1920, Just $ int 1080))
                         . (textFontSize ?~ float 16)
                         . (textAlign .~ iv2 (0, 0))
                         ) (str t)

-- Geometry generators and utilities

sinC i = waveC' (waveCNames ?~ str "rz") i $ osin (castf sampleIndex) !* float 360
scaleC i n = waveC' (waveCNames ?~ str "sx") i $ castf sampleIndex !* n

sidesTorus sides scale = torus' ((torusOrientation ?~ int 2) . (torusRows ?~ int 10) . (torusColumns ?~ sides) . (torusRadius .~ v2 (scale !* float 1) (scale !* float 0.5)))

lineLines width scale inst sop =
  let
    instances = casti inst !+ int 2
  in
    lineGeo (math' (mathMult ?~ scale) [ain]) (sinC instances) (scaleC instances $ float 0.1) (scaleC instances $ float 0.1) sop width instances wireframeM

lineGeo ty rz sx sy sop width instances mat =
  let
    ain = math' (mathMult ?~ float 10) [audioIn]
    sgeo = instanceGeo' ((geoMat ?~ mat)) poses (outS $ sop)
    poses = mergeC' (mergeCAlign ?~ int 7) [tx, ty', rz & renameC (str "rz"), sx & renameC (str "sx"), sy & renameC (str "sy")]
    tx = waveC' (waveCNames ?~ str "tx") instances $ ((castf (sampleIndex !* casti width !* int 2)) !/ castf instances) !- width
    ty' = ty & resampleC' ((resampleEnd ?~ instances) . (resampleRate ?~ instances)) False & renameC (str "ty")
    centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
    volume = analyze (int 6) ain
    volc = chan0f volume
  in
    render [sgeo] (centerCam (v3 (float 0) (float 0) (float 50)) emptyV3)

spiralGeo inst speed sop =
  let
    sgeo = instanceGeo' ((geoMat ?~ wireframeM) . (geoUniformScale ?~ float 0.1)) poses (outS $ sop)
    instances = casti $ inst
    poses = mergeC' (mergeCAlign ?~ int 7) [ty, tx, tz]
    instanceIter n = (castf sampleIndex !+ (speed !* float n) !% castf instances)
    tx = waveC' (waveCNames ?~ str "tx") instances $ ocos (castf sampleIndex !* float 60 !+ instanceIter 0.2) !* ((instanceIter 10 !* float 0.1) !+ float 4)
    ty = waveC' (waveCNames ?~ str "ty") instances $ osin (castf sampleIndex !* float 60 !+ instanceIter 0.2) !* ((instanceIter 10 !* float 0.1) !+ float 4)
    tz = waveC' (waveCNames ?~ str "tz") instances $ instanceIter 10 !* float 1 !- float 50
    centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
  in
    render [sgeo] (centerCam (v3 (float 0) (float 0) (float 5)) emptyV3)

-- vidIn

-- Effects

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id
over' f l o t = feedbackT t (\t' -> l $ compT 32 [t, levelT' (levelOpacity ?~ o) t']) f
over = over' id id
brightness' f b = levelT' (levelBrightness ?~ b)
brightness = brightness' id
--blur
crosshatch' f = frag' f "crosshatch.frag" [] . (:[])
paletterepeatT' f p r top = frag' f "color_repeat.frag" [("i_repeat", xV4 r)] [top, palette p]
paletterepeatT = paletterepeatT' id
edgesc' f c t = compT 0 [edges' f t, levelT' (levelOpacity ?~ c) t]
edgesc = edgesc' id
flowermod s = frag' id "flower_mod.frag" [("uSeconds", xV4 s)] . (:[])
hue h = hsvT' (hsvAdjHueOffset ?~ h)
littleplanet' f = frag' f "little_planet.frag" [] . (:[])
littleplanet = littleplanet' id
lumidots' f = frag' f "lumidots.frag" [] . (:[])
lumidots = lumidots' id
mirror' f t = compT' f 0 [flipT' ((flipx ?~ bool True) . (flipy ?~ bool True)) t, t]
mirror = mirror' id
mosaic' f t s top = frag' f "mosaic.frag" [("uTime", xV4 t), ("uScale", xV4 s)] [top]
mosaic = mosaic' id
noisedisplace' f d top = frag' f "noise_displace.frag" [("uTime", xV4 seconds), ("uDisplacement", xV4 d)] [top]
noisedisplace = noisedisplace' id
palettecycle' f p s t = compT' f 27 [crop' ((cropLeft ?~ s) . (cropRight ?~ s)) $ palette p, t]
palettecycle = palettecycle' id
palettemap' f p o t = frag' f "palette_map.frag" [("uOffset", xV4 o), ("uSamples", xV4 $ float 16)] [t, palette p]
palettemap = palettemap' id
repeatT' f s = transformscale' f s 2
repeatT = repeatT' id
repeatTxy' f r = repeatT' f (Just r, Just r)
repeatTxy = repeatTxy' id
rgbsplit' s top = frag' id "rgbsplit.frag" [("uFrames", xV4 s)] [top]
rotate' f r = transformT' (transformRotate ?~ r)
rotate = rotate' id
sat s = hsvT' (hsvAdjSatMult ?~ s)
scale' f s = transformscale' f s 1
scale = scale' id
scalexy' f s = scale' f (Just s, Just s)
scalexy = scalexy' id
strobe' f s top = frag' f "strobe.frag" [("uSpeed", xV4 s), ("uTime", xV4 seconds)] [top]
strobe = strobe' id
transformext' f e = transformT' (f . (transformExtend ?~ (int e)))
transformscale' f s e = transformext' (f . (transformScale .~ ((!^ (float (-1))) <$> fst s, (!^ (float (-1))) <$> snd s) )) e
transformscale = transformscale' id
translate' f t = transformT' (f . (transformExtend ?~ int 3) . (transformTranslate .~ t))
translate (a, b) = translate' id (Just a, Just b)
translatex' f x = translate' f $ (Just x, Just $ float 0)
translatex = translatex' id
translatey' f y = translate' f $ (Just $ float 0, Just y)
translatey = translatey' id
val v = hsvT' (hsvAdjValMult ?~ v)

clone c (tx, ty) (sx, sy) top = frag' id "clone.frag" [("uClones", xV4 c), ("uTranslate", emptyV4 & _1 ?~ tx & _2 ?~ ty), ("uScale", emptyV4 & _1 ?~ sx & _2 ?~ sy)] [top]

-- combiners

addops = compT 0
fadeops f = switchT' (switchTBlend ?~ bool True) f
multops = compT 27
overops = compT 31
triggercount f l = count' ((countThresh ?~ float 0.5) .
                                     (countLimMax ?~ float (fromIntegral $ l)) .
                                     (countLimType ?~ int 1)
                                    ) f
triggerops f tops = switchT (chan0f $ triggercount f (length tops - 1)) tops
showwork g es =
  let
    scans = scanl (\g e -> e g) g es
  in
    addops $ (head $ reverse scans):(zipWith (\i -> (translate (float (-0.45) !+ (float 0.1 !* float i), float 0.4)) . scalexy (float 10)) [0..] (take (length es) $ scans))

-- led
(>>>) = flip (.)
constled f n = constC' (constCEndFrames ?~ int n) [f]
rgbled n r g b = mergeC [r & stretchC (int n), g & stretchC (int n), b & stretchC (int n)]
               & shuffleC (int 6) 
topToLed n = 
    crop' ((cropTop ?~ float 0.00008))
    >>> topToC
    >>> selectC' (selectCNames ?~ str "r g b")
    >>> stretchC (int n)
    >>> shuffleC (int 6)
bottomLedSplit n t = (topToLed n t, t)
ledPalette p = flip lookupC (topToC' (topToChopAName ?~ str "") $ palette p)

-- palettes

data Palette = Palette [Color]
data Color = Hex BS.ByteString | RGB Int Int Int
neon = Palette $ Hex <$> ["A9336B", "5F2F88", "CB673D", "87BB38"]
fire = Palette $ Hex . BS.pack . fmap toUpper <$> ["f07f13", "800909", "f27d0c", "fdcf58"]
buddhist = Palette $ Hex . BS.pack . fmap toUpper <$> ["0000FF", "FFFF00", "FF0000", "FFFFFF", "FF9800"]
tealcontrast = Palette [RGB 188 242 246, RGB 50 107 113, RGB 211 90 30, RGB 209 122 43, RGB 188 242 246]
purplish = Palette [RGB 150 110 100, RGB 223 143 67, RGB 76 73 100 , RGB 146 118 133, RGB 165 148 180]
sunset = Palette [RGB 185 117 19, RGB 228 187 108, RGB 251 162 1, RGB 255 243 201]
coolpink = Palette [RGB 215 40 26, RGB 157 60 121, RGB 179 83 154, RGB 187 59 98]
darkestred = Palette [RGB 153 7 17, RGB 97 6 11, RGB 49 7 8, RGB 13 7 7, RGB 189 5 13]
nature = Palette [RGB 63 124 7, RGB 201 121 66, RGB 213 101 23, RGB 177 201 80, RGB 180 207 127]
greenpurple = Palette [RGB 42 4 74, RGB 11 46 89, RGB 13 103 89, RGB 122 179 23, RGB 160 197 95]

------------------------

lagmod l = chan0f . lag (float 0) (float l) . constC . (:[])
tres = (topResolution .~ (Just $ int 1920, Just $ int 1080)) . (pixelFormat ?~ int 3)
scr = (++) "scripts/Visuals/"
frag = frag' id
frag' f s = glslTP' (tres . f) (scr s)
rendered g = render' (renderLight .~ [light]) [g] cam
tdata v t = frag "audio_data.frag" [("i_volume", xV4 v)] [t]
palette (Palette colors) = ramp' (topResolution .~ iv2 (128, 0)) . table
  . fromLists $ ["r", "g", "b", "a", "pos"]:(zipWith (colorToBS (length colors)) [0..] colors)

colorToBS :: Int -> Int -> Color -> [BS.ByteString]
colorToBS n i (Hex str) =
  let
    hexes = chunksOf 2 . drop 1
    todig = flip L.elemIndex "0123456789ABCDEF"
    toIntList = fmap todig
    toInt = foldr (\i acc -> acc * 16 + i) 0
    toHex = fmap toInt . sequence . toIntList
    hextorgb = fmap (BS.pack . show . (/ 256) . fromIntegral)
  in
    catMaybes $ (hextorgb <$> (toHex <$> hexes (show str))) ++ [Just "1.0", Just . BS.pack . show $ fromIntegral i / fromIntegral n]
colorToBS n i (RGB r g b) =
  (++ [BS.pack . show $ fromIntegral i / fromIntegral n]) $ fmap (BS.pack . show . (/ 256) . fromIntegral) [r, g, b, 256]


------------------------------
-- Randos

tidalmessages = oscinD' (oscInDAddressScope ?~ str "/vis") 7010 & selectD' ((selectDRStartI ?~ int 1) . (selectDREndI ?~ int 1) . (selectDCStartI ?~ int 5) . (selectDCEndI ?~ int 5))
tidalvis = tox "toxes/tidal_patterns.tox" [] (Just tidalmessages)
tidalvistop = tidalvis & nullC & chopToT' (chopToTopFormat ?~ int 2)
module Network where

import Control.Lens
import LambdaDesigner.Lib
import LambdaDesigner.Op
import LambdaDesigner.ParsedOps
import Visuals

import Data.ByteString.Char8 as BS
import Data.List as L

data Beat = 
    Beat 
        { beatpulse :: Tree CHOP
        , bps :: Tree CHOP
        }

tapbeat :: Tree Float -> Beat
tapbeat i = 
  let
    const1 = constantCHOP (constantCHOPvalue0 ?~ float 1) []
    beat = constantCHOP (constantCHOPvalue0 ?~ i) [] & logicCHOP (logicCHOPpreop ?~ int 5) . (:[])
    beathold = holdCHOP id [speedCHOP id [const1, beat] & delayCHOP ((delayCHOPdelay ?~ float 1) . (delayCHOPdelayunit ?~ int 1)), beat] & nullCHOP (nullCHOPcooktype ?~ int 2)
    beattrail = 
        trailCHOP ((trailCHOPwlength ?~ float 8) . (trailCHOPwlengthunit ?~ int 1) . (trailCHOPcapture ?~ int 1)) [beathold]
        & deleteCHOP ((deleteCHOPdelsamples ?~ bool True) . (deleteCHOPcondition ?~ int 5) . (deleteCHOPinclvalue1 ?~ bool False)) . (:[])
    bps = mathCHOP (mathCHOPpostop ?~ int 5) . (:[]) $ analyze 0 beattrail
    beataccum = speedCHOP id [bps, beat]
    finalbeat = 
        beataccum 
            & limitCHOP ((limitCHOPmax ?~ float 1) . (limitCHOPtype ?~ int 2) . (limitCHOPmin ?~ float 0)) 
            & logicCHOP ((logicCHOPboundmax ?~ float 0.08) . (logicCHOPpreop ?~ int 5) . (logicCHOPconvert ?~ int 2)) . (:[])
  in
    Beat finalbeat bps

beatramp :: Beat -> Tree CHOP
beatramp (Beat beat bps) = speedCHOP (speedCHOPresetcondition ?~ int 2) [bps, beat]

beatxcount :: Float -> Tree CHOP -> Beat -> Tree CHOP
beatxcount x reset (Beat beat _) = countCHOP ((countCHOPoutput ?~ int 1) . (countCHOPlimitmax ?~ float (x - 1))) [beat, reset]

beatxpulse :: Float -> Tree CHOP -> Beat -> Tree CHOP
beatxpulse x reset = logicCHOP (logicCHOPpreop ?~ int 6) . (:[]) . beatxcount x reset

beatxramp :: Float -> Tree CHOP -> Beat -> Tree CHOP
beatxramp x reset beat@(Beat bpulse bps) = speedCHOP id [bps & mathCHOP (mathCHOPgain ?~ float (1/x)) . (:[]), beatxpulse x reset beat]


midi = midiinCHOP (
        (midiinCHOPid ?~ str "2") . 
        (midiinCHOPsimplified ?~ bool False) . 
        (midiinCHOPpulsename ?~ str "beat") .
        (midiinCHOPchannel ?~ str "1-8") .
        (midiinCHOPcontrolind ?~ str "1-16")
        )

midisyncbeat = Network.tapbeat (chan0f $ Network.midi & selectCHOP (selectCHOPchannames ?~ str "beat") . (:[]))

    -- Beat 
    --     () 
    --     (Network.midi 
    --         & selectCHOP (selectCHOPchannames ?~ str "ch1c5") . (:[]) 
    --         & mathCHOP ((mathCHOPgain ?~ float (127/60)) . (mathCHOPpreoff ?~ float (60/128))) . (:[]))

mchop = midichanop


network :: BS.ByteString
network =
    -- let
    --     const1 = constantCHOP (constantCHOPvalue0 ?~ float 1) []
    --     dmx = dmxoutCHOP ((dmxoutCHOPinterface ?~ int 3) . (dmxoutCHOPnetaddress ?~ str "10.7.224.159") . (dmxoutCHOPlocaladdress ?~ str "10.7.224.158") . (dmxoutCHOPrate ?~ int 40)) $ mathCHOP (mathCHOPtorange2 ?~ float 255) [const1, const1, const1, const1, const1]
    -- in
    --     printMessages $ compile ([dmx]) ([outTOP id $ constantTOP (constantTOPalpha ?~ float 0.25 !* (chan0f barramp)) []] :: [Tree TOP]) mempty 
    let
        -- midi = midiinCHOP ((midiinCHOPid ?~ str "2") . (midiinCHOPsimplified ?~ bool False) . (midiinCHOPpulsename ?~ str "beat"))
        -- beat = midi & selectCHOP (selectCHOPchannames ?~ str "beat") . (:[])
        -- const1 = constantCHOP (constantCHOPvalue0 ?~ float 1) []
        -- bar = countCHOP ((countCHOPoutput ?~ int 1) . (countCHOPlimitmax ?~ float 3)) [beat]
        -- beatramp = speedCHOP id [const1, beat]
        -- dividebyone c = mathCHOP (mathCHOPchopop ?~ int 4) [const1, c]
        -- barramp = speedCHOP (speedCHOPresetcondition ?~ int 2) [beatramp & trailCHOP id . (:[]) & analyzeCHOP (analyzeCHOPfunction ?~ int 1) & dividebyone, bar] 

        -- -- beatpulse = midi & selectCHOP (selectCHOPchannames ?~ str "beat") . (:[])
        -- -- beatxpulse x reset = countCHOP ((countCHOPoutput ?~ int 1) . (countCHOPlimitmax ?~ float (x - 1))) [beatpulse, reset] & logicCHOP (logicCHOPpreop ?~ int 6) . (:[]) 
        -- -- barpulse = beatxpulse 4
        -- beatxoneshot x b input = 
        --     logicCHOP (logicCHOPchopop ?~ int 1)
        --     [ countCHOP ((countCHOPoutput ?~ int 2) . (countCHOPlimitmax ?~ float (x))) [b, triggered b input]  
        --         & logicCHOP ((logicCHOPboundmax ?~ float (x - 1)) . (logicCHOPconvert ?~ int 2)) . (:[]) 
        --     , triggered b input & countCHOP ((countCHOPoutput ?~ int 2) . (countCHOPlimitmax ?~ float 1)) . (:[])
        --     ]
        -- rampxoneshot x b input =
        --     let
        --         bos = beatxoneshot x b input 
        --         tog = logicCHOP (logicCHOPpreop ?~ int 2) [bos]
        --         negpos = mathCHOP (mathCHOPtorange1 ?~ float (-1)) [tog]
        --         togneg = mathCHOP (mathCHOPchopop ?~ int 3) [bos, negpos]
        --     in
        --         speedCHOP (speedCHOPresetcondition ?~ int 0) [togneg, tog]


        -- midiout c = midioutCHOP ((midioutCHOPid ?~ str "2")) [c]
        -- midiLC = midiinmapCHOP ((midiinmapCHOPid ?~ str "1"))
        -- maps1 = midiLC & selectCHOP ((selectCHOPchannames ?~ str "b2 s2 b3 s3") . (selectCHOPrenameto ?~ str (L.intercalate " " [deckAmod playpause, deckAmod vol, deckBmod playpause, deckBmod vol]))) . (:[]) & midiout
        -- playpause = "c1"
        -- vol = "c2"
        -- deckAmod = (++) "ch1"
        -- deckBmod = (++) "ch2"


        -- triggered b input = 
        --     logicCHOP (logicCHOPchopop ?~ int 2) 
        --         [ countCHOP ((countCHOPlimitmax ?~ float 1) . (countCHOPoutput ?~ int 1)) [input, b] 
        --         , logicCHOP (logicCHOPchopop ?~ int 1) [b, input]
        --         ] 
        --     & logicCHOP (logicCHOPpreop ?~ int 6) . (:[]) 
        -- beatTrigger = triggered beat 
        -- barTrigger = triggered bar 
        tappedbeat = Network.tapbeat $ (mchan "b9")
        tappedbarramp = beatxramp 4 (mchop "b10") tappedbeat
        midisyncbarramp = beatxramp 4 (mchop "b10") midisyncbeat
    in
        printMessages $ compile ([outCHOP id $ midisyncbarramp]) ([outTOP id $ constantTOP (constantTOPalpha ?~ chan0f midisyncbarramp) []]) mempty 

-- sensel
    -- let 
    --     sensel = cplusplusCHOP ( (cplusplusCHOPplugin ?~ str" "C:/Users/ulyssesp/Development/SenselCHOP/Release/CPlusPlusCHOPExample.dll")) []
    --     senselchop = sensel & selectCHOP (selectCHOPchannames ?~ str "chan") . (:[]) & shuffleCHOP ((shuffleCHOPmethod ?~ int 8) . (shuffleCHOPnval ?~ int 185)) 
    --     senseltop = choptoTOP (choptoTOPchop ?~ senselchop) & flipTOP (flipTOPflipy ?~ bool True)
    --     senseltouches = sensel & selectCHOP (selectCHOPchannames ?~ str chan1") . (:[]) & deleteCHOP ((deleteCHOPdelsamples ?~ bool True)) . (:[])
    -- in 

-- endTops :: (Tree TOP, Tree CHOP)
    --     printMessages $ compile ([outCHOP id $ senseltouches]) ([outTOP id $ fade (float 0.97) senseltop]) mempty

-- endTops = 
--     ( tgal & repeatTxy (float 10)
--     , midi 

-- tgal = shapes (float 9) (float 0.6) (float 0.5) 
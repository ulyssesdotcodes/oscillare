module Network where

import Control.Lens
import LambdaDesigner.Lib
import LambdaDesigner.Op
import LambdaDesigner.ParsedOps
import Visuals

import Data.ByteString.Char8 as BS
import Data.List as L

network :: BS.ByteString
network =
    let
        midi = midiinCHOP ((midiinCHOPid ?~ str "2") . (midiinCHOPsimplified ?~ bool False) . (midiinCHOPpulsename ?~ str "beat"))
        beat = midi & selectCHOP (selectCHOPchannames ?~ str "beat") . (:[])
        const1 = constantCHOP (constantCHOPvalue0 ?~ float 1) []
        bar = countCHOP ((countCHOPoutput ?~ int 1) . (countCHOPlimitmax ?~ float 3)) [beat]
        beatramp = speedCHOP id [const1, beat]
        dividebyone c = mathCHOP (mathCHOPchopop ?~ int 4) [const1, c]
        barramp = speedCHOP (speedCHOPresetcondition ?~ int 2) [beatramp & trailCHOP id . (:[]) & analyzeCHOP (analyzeCHOPfunction ?~ int 1) & dividebyone, bar] 

        beatpulse = midi & selectCHOP (selectCHOPchannames ?~ str "beat") . (:[])
        beatxpulse x reset = countCHOP ((countCHOPoutput ?~ int 1) . (countCHOPlimitmax ?~ float (x - 1))) [beatpulse, reset] & logicCHOP (logicCHOPpreop ?~ int 6) . (:[]) 
        barpulse = beatxpulse 4
        beatxoneshot x b input = 
            logicCHOP (logicCHOPchopop ?~ int 1)
            [ countCHOP ((countCHOPoutput ?~ int 2) . (countCHOPlimitmax ?~ float (x))) [b, triggered b input]  
                & logicCHOP ((logicCHOPboundmax ?~ float (x - 1)) . (logicCHOPconvert ?~ int 2)) . (:[]) 
            , triggered b input & countCHOP ((countCHOPoutput ?~ int 2) . (countCHOPlimitmax ?~ float 1)) . (:[])
            ]
        rampxoneshot x b input =
            let
                bos = beatxoneshot x b input 
                tog = logicCHOP (logicCHOPpreop ?~ int 2) [bos]
                negpos = mathCHOP (mathCHOPtorange1 ?~ float (-1)) [tog]
                togneg = mathCHOP (mathCHOPchopop ?~ int 3) [bos, negpos]
            in
                speedCHOP (speedCHOPresetcondition ?~ int 0) [togneg, tog]


        midiout c = midioutCHOP ((midioutCHOPid ?~ str "2")) [c]
        midiLC = midiinmapCHOP ((midiinmapCHOPid ?~ str "1"))
        maps1 = midiLC & selectCHOP ((selectCHOPchannames ?~ str "b2 s2 b3 s3") . (selectCHOPrenameto ?~ str (L.intercalate " " [deckAmod playpause, deckAmod vol, deckBmod playpause, deckBmod vol]))) . (:[]) & midiout
        playpause = "c1"
        vol = "c2"
        deckAmod = (++) "ch1"
        deckBmod = (++) "ch2"


        triggered b input = 
            logicCHOP (logicCHOPchopop ?~ int 2) 
                [ countCHOP ((countCHOPlimitmax ?~ float 1) . (countCHOPoutput ?~ int 1)) [input, b] 
                , logicCHOP (logicCHOPchopop ?~ int 1) [b, input]
                ] 
            & logicCHOP (logicCHOPpreop ?~ int 6) . (:[]) 
        beatTrigger = triggered beat 
        barTrigger = triggered bar 
    in
        printMessages $ compile ([outCHOP id $ barramp, rampxoneshot 8 (tapbeat $ mchan "b9") (midiLC & selectCHOP ((selectCHOPchannames ?~ str "b10")) . (:[]))]) ([outTOP id $ constantTOP (constantTOPalpha ?~ float 0.25 !* (chan0f barramp)) []] :: [Tree TOP]) mempty 


-- sensel
    -- let 
    --     sensel = cplusplusCHOP ( (cplusplusCHOPplugin ?~ str "C:/Users/ulyssesp/Development/SenselCHOP/Release/CPlusPlusCHOPExample.dll")) []
    --     senselchop = sensel & selectCHOP (selectCHOPchannames ?~ str "chan") . (:[]) & shuffleCHOP ((shuffleCHOPmethod ?~ int 8) . (shuffleCHOPnval ?~ int 185)) 
    --     senseltop = choptoTOP (choptoTOPchop ?~ senselchop) & flipTOP (flipTOPflipy ?~ bool True)
    --     senseltouches = sensel & selectCHOP (selectCHOPchannames ?~ str "chan1") . (:[]) & deleteCHOP ((deleteCHOPdelsamples ?~ bool True)) . (:[])
    -- in 
    --     printMessages $ compile ([outCHOP id $ senseltouches]) ([outTOP id $ fade (float 0.97) senseltop]) mempty


-- endTops :: (Tree TOP, Tree CHOP)
-- endTops = 
--     ( tgal & repeatTxy (float 10)
--     , midi 

-- tgal = shapes (float 9) (float 0.6) (float 0.5) 
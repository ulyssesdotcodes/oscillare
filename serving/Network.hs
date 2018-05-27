module Network where

import Control.Lens
import LambdaDesigner.Lib
import LambdaDesigner.Op
import Visuals

import Data.ByteString.Char8 as BS

network :: BS.ByteString
network = printMessages $ compile ([] :: [Tree TOP]) ((:[]) . outT $ endTop) mempty

endTop :: Tree TOP
endTop = tgal 
         & repeatTxy (float 10)

tgal = shapes (float 8) (float 0.6) (float 0.2) 
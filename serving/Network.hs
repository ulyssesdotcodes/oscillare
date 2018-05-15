module Network where

import LambdaDesigner.Lib
import LambdaDesigner.Op
import Visuals

import Data.ByteString.Char8 as BS

network :: BS.ByteString
network = printMessages $ compile ([] :: [Tree TOP]) ((:[]) . outT $ endTop) mempty

endTop :: Tree TOP
endTop = shapes (float 3) (seconds !% float 0.3) (float 0.2) 




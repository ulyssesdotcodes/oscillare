module Params where

import Pattern
import Stream
import Utils

import qualified Data.Map as Map

make' :: (a -> Value) -> Param -> Pattern a -> ParamPattern
make' toValue par p = fmap (\x -> Map.singleton par (defaultV x)) p
  where defaultV a = Just $ toValue a

grp :: [Param] -> Pattern String -> ParamPattern
grp [] _ = silence
grp params p = (fmap lookupPattern p)
  where lookupPattern :: String -> ParamMap
        lookupPattern s = Map.fromList $ map (\(param,s') -> toPV param s') $ zip params $ (split s)
        split s = wordsBy (==':') s
        toPV :: Param -> String -> (Param, Maybe Value)
        toPV param@(S _ _) s = (param, (Just $ VS s))
        toPV param@(F _ _) s = (param, (Just $ VF $ read s))

prog :: Pattern String -> ParamPattern
prog = p_p

pS name defaultV = (make' VS param, param)
  where param = S name defaultV

(p, p_p) = pS "p" Nothing

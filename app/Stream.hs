{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}

module Stream where

import Pattern
import Tempo

import qualified Data.Map as Map

type ToMessageFunc = [Param] -> Tempo -> Int -> (Double, ParamMap) -> Maybe (IO ())

data Backend a = { toMessage :: ToMessageFunc
                 , flush :: [Param] -> Tempo -> Int -> IO ()
                 }

data Param = S { name :: String, sDefault :: Maybe String }
           | F { name :: String, fDefault :: Maybe Float }

instance Eq Param where
  a == b = name a == name b


data Value = VS { svalue :: String } | VF { fvalue :: Float }
  deriving (Show, Eq, Ord)

type ParamMap = Map.Map Param (Maybe Value)

type ParamPattern = Pattern ParamMap

ticksPerCycle = 8

defaultValue :: Param -> Maybe Value
defaultValue (S _ (Just x)) = Just $ VS x
defaultValue (F _ (Just x)) = Just $ VF x
defaultValue _ = Nothing

hasDefault :: Param -> Bool
hasDefault (S _ Nothing) = False
hasDefault (F _ Nothing) = False
hasDefault _ = True

defaulted :: [Param] -> [Param]
defaulted = filter hasDefault

defaultMap :: [Param] -> ParamMap
defaultMap ps = Map.fromList $ map (\x -> (x, defaultValue x)) (defaulted ps)


start :: Backend a -> [Param] -> IO (MVar (ParamPattern))
start backend shape
  = do patternM <- newVar silence
       let ot = (onTick backend shape patternM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternM

onTick :: Backend a -> [Param] -> MVar (ParamPattern) -> Tempo -> Int -> IO ()
onTick backend shape patternM change ticks
  = do p <- readMVar patternM
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe
                      (toMessage backend shape change ticks)
                      (seqToRelOnsets (a, b) p)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       flush backend shape change ticks
       return ()

{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}

module Stream where

data Param = S { name :: String, sDefault :: Maybe String }
           | F { name :: String, fDefault :: Maybe Float }

instance Eq Param where
  a == b = name a == name b


data Value = VS { svalue :: String } | VF { fvalue :: Float }
  deriving (Show, Eq, Ord)

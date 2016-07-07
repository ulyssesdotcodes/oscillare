module Utils where

mapThd' :: (c -> x) -> (a, b, c) -> (a, b, x)
mapThd' f (a, b , c) = (a, b, f c)

thd' (_, _, c) = c

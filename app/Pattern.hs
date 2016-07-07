module Pattern where

import Data.Ratio

import Time
import Utils

data Pattern a = Pattern { arc :: Arc -> [Event a] }

instance Functor Pattern where
  fmap f (Pattern a) = Pattern $ fmap (fmap (mapThd' f)) a

instance Applicative Pattern where
  pure x = Pattern $ \(s, e) -> map
                                 (\t -> ((t%1, (t+1)%1),
                                         (t%1, (t+1)%1),
                                         x
                                        )
                                 )
                                 [floor s .. (ceiling e - 1)]
  (Pattern fs) <*> (Pattern xs) =
    Pattern $ \a -> concatMap applyX (fs a)
    where applyX ((s, e), (s', e'), f) =
            map (\(_, _, x) -> ((s, e), (s', e'), f x))
                (filter
                 (\(_, a', _) -> isIn a' s)
                 (xs, (s', e'))
                )

instance Monoid (Pattern a) where
  mempty = blank
  mappend = overlay

instance Monad Pattern where
  return = pure
  p >>= f = unwrap (f <$> p)

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap p = Pattern $ \a -> concatMap ((\p' -> arc p' a) . thd') (arc p a)

blank :: Pattern a
blank = Pattern $ const []

overlay :: Pattern a -> Pattern a -> Pattern a
overlay p p' = Pattern $ (\a -> (arc p a) ++ (arc p' a))

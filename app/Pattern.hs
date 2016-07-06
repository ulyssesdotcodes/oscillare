module Pattern where

import Time

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

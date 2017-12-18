{-# LANGUAGE CPP           #-}
module Debug.Hoed.Compat (sortOn, (<$), (<$>)) where

import Control.Applicative
import Data.List

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))
#endif

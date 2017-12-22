{-# LANGUAGE CPP #-}
module Debug.Hoed.Compat (addConstraint, sortOn, (<$), (<$>)) where

import           Control.Applicative
import           Data.List
import           Language.Haskell.TH

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))
#endif

addConstraint :: Name -> [Type] -> Pred
addConstraint name args =
#if __GLASGOW_HASKELL__ < 710
      ClassP name args
#else
      foldl' AppT (ConT name) args
#endif

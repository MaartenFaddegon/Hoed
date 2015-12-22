module Utils where

import qualified Data.Map as Map
import Data.Map(Map)

foldlWithKeyM :: Monad m => (a -> k -> b -> m a) -> a -> Map k b -> m a
foldlWithKeyM f acc = Map.foldlWithKey f' (return acc)
    where
        f' ma k b = ma >>= \a -> f a k b
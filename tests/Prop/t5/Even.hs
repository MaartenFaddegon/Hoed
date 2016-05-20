module Even where

import Prelude hiding (filter)
import Debug.Hoed.Pure
import Data.Bits
import Properties
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property hiding ((.&.))
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.Maybe

filter :: Observable a => (a -> Bool) -> [a] -> [a]
filter = observe "filter" filter'
filter' pred []    = []
filter' pred (x:xs)
  | pred x         = filter pred xs
  | otherwise      = x : filter pred xs

evens :: [Int] -> [Int]
evens = observe "evens" evens'
evens' xs = filter isEven xs

isEven :: Int -> Bool
isEven = observe "isEven" isEven'
isEven' x = (x .&. 1) == 0

doit :: IO ()
doit = logOwp Bottom "hoed-tests-Prop-t5.graph" properties ((\q -> do MkRose res ts <- reduceRose .  unProp . (\p->unGen p  (mkQCGen 1) 1) . unProperty $ q; print . fromJust . ok $ res) $ spec_evens evens [2,3] 2)

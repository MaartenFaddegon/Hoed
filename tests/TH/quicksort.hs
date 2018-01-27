{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.List (partition)
import Debug.Hoed
import Debug.Hoed.TH
import System.Process
import System.Exit

obs [d|
  quicksort :: (a -> a -> Bool) -> [a] -> [a]
  quicksort op [] = []
  quicksort op (x:xs) = quicksort op lt ++ [x] ++ quicksort op gt
    where (lt, gt) = partition (`op` x) xs
        |]

debug [d|
  quicksort' :: (a -> a -> Bool) -> [a] -> [a]
  quicksort' op [] = []
  quicksort' op (x:xs) = quicksort' op lt ++ [x] ++ quicksort' op gt
    where (lt, gt) = partition (`op` x) xs
        |]

main = logO "hoed-tests-th-quicksort.graph" $ do
  print $ quicksort  (<) "haskell"
  print $ quicksort' (<) "haskell"
  exitWith =<< system "diff hoed-tests-th-quicksort.graph tests/ref/hoed-tests-th-quicksort.graph"

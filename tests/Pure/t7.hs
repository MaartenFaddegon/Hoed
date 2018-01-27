{-# LANGUAGE OverloadedStrings #-}
import Debug.Hoed
import System.Process(system)
import System.Exit(exitWith)

plus :: Int -> Int -> Int
plus = {- observe "plus"-} (+)

apx :: (Int -> Int) -> Int -> Int
apx = {- observe "apx" -} apx'
apx' f x = f x

apxy :: (Int -> Int -> Int) -> Int -> Int -> Int
apxy = observe "apxy" apxy'
apxy' f x y = f x y

ap45' :: (Int->Int->Int) -> Int
ap45 = observe "ap45" ap45'
ap45' f = apx (apxy f 4) 5

main = do
  logO "hoed-tests-Pure-t7.graph" $ print (ap45 plus)
  i <- system "diff hoed-tests-Pure-t7.graph tests/ref/hoed-tests-Pure-t7.graph"
  exitWith i

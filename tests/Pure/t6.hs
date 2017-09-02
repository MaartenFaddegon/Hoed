{-# LANGUAGE DeriveGeneric #-}
module Main where
import Debug.Hoed

-- This test demonstrates that using the FPretty library we pretty print
-- much bigger computation statements than with the previous implementation
-- based on Wadler's "prettier printer".

data T = Step T | End deriving Generic
instance Observable T

v :: T
v = foldr (\_ -> Step) End [1..1000]

ends :: T -> Bool
ends = observe "ends" ends'
ends' (Step t) = ends' t
ends' End      = True

main = logO "hoed-tests-Pure-t6.graph" $ print $ ends v

module Main where

import Test.Hspec
import qualified Data.Rope.Mutable.Spec as Rope

main :: IO ()
main = hspec spec

spec = do
  Rope.spec

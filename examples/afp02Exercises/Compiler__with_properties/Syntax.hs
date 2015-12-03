{-# LANGUAGE DeriveGeneric #-}
module Syntax(Name, Expr(..), Command(..)) where

import Value
import Debug.Hoed.Pure
import Test.QuickCheck
import Control.Monad(liftM,liftM2,liftM3)

type Name = String

name :: Gen Name
name = elements ["x","y","z"]

data Expr
  = Var Name
  | Val Value
  | Uno Op1 Expr
  | Duo Op2 Expr Expr
  deriving (Eq, Show, Generic)

instance Observable Expr

instance Arbitrary Expr where
  arbitrary = sized expr
    where expr 0 = oneof [liftM Var name, liftM Val arbitrary]
          expr n = oneof [liftM Var name, liftM Val arbitrary
                         ,liftM2 Uno arbitrary (expr (n-1))
                         ,liftM3 Duo arbitrary (expr (n `div` 2)) (expr (n `div` 2))]

data Command
  = Skip
  | Name := Expr
  | Command :-> Command
  | If Expr Command Command
  | While Expr Command
  | Print Expr
  deriving (Eq, Show, Generic)

instance Observable Command

instance Arbitrary Command where
  arbitrary = sized command
    where command 0 = oneof [liftM2 (:=) name arbitrary, return Skip]
          command n = oneof [liftM2 (:=) name arbitrary, return Skip
                            ,liftM2 (:->) (command (n `div` 2)) (command (n `div` 2))
                            ,liftM3 If arbitrary (command (n `div` 2)) (command (n `div` 2))
                            ,liftM2 While arbitrary (command (n-1))
                            ,liftM  Print arbitrary]


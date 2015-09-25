{-# LANGUAGE DeriveGeneric #-}
module Syntax(Name, Expr(..), Command(..)) where

import Value
import Debug.Hoed.Pure

type Name = String

data Expr
  = Var Name
  | Val Value
  | Uno Op1 Expr
  | Duo Op2 Expr Expr
  deriving (Eq, Show, Generic)

instance Observable Expr

data Command
  = Skip
  | Name := Expr
  | Command :-> Command
  | If Expr Command Command
  | While Expr Command
  | Print Expr
  deriving (Eq, Show, Generic)

instance Observable Command

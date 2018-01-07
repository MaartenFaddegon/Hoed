{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Debug.Hoed.Fields where

import GHC.Generics
import GHC.Exts
import GHC.TypeLits (ErrorMessage(..), TypeError)

data Nat = Z | S Nat

-- A constraint on the number of the constructors in a datatype
type family FieldLimit (n :: Nat) a :: Constraint where
  FieldLimit n (M1 c meta f) = FieldLimit n f
  FieldLimit n (f :+: g)     = (FieldLimit n f, FieldLimit n g)
  FieldLimit ('S n) (f :*: g) = FieldLimit n g
  FieldLimit n U1 = ()
  FieldLimit n V1 = ()
  FieldLimit n (K1 _ _) = ()
  FieldLimit n (URec _) = ()
  FieldLimit 'Z f = TypeError ('Text "Hoed only handles constructors with 64 fields or less")
{-# LANGUAGE DeriveGeneric #-}

module CNF where
import Debug.Hoed.Pure hiding (O)
import Data.List

data Prop = V Char | N Prop | O Prop Prop | A Prop Prop
            deriving (Eq, Show, Generic)

instance Observable Prop

anywhere :: (Prop -> Bool) -> Prop -> Bool
anywhere f p  =  f p || anywhere' f p

anywhere' :: (Prop -> Bool) -> Prop -> Bool
anywhere' f (N p)    =  anywhere f p
anywhere' f (O p q)  =  anywhere f p || anywhere f q
anywhere' f (A p q)  =  anywhere f p || anywhere f q
anywhere' _ _        =  False

vars :: Prop -> [Char]
vars (V v)    =  [v]
vars (N p)    =  vars p
vars (O p q)  =  union (vars p) (vars q)
vars (A p q)  =  union (vars p) (vars q)

equiv :: Prop -> Prop -> Bool
equiv p q  =  all (\i -> eval i p == eval i q) (interps vs)
  where
  vs  =  union (vars p) (vars q)

eval :: [Char] -> Prop -> Bool
eval i (V v)    =  elem v i
eval i (N p)    =  not (eval i p)
eval i (O p q)  =  eval i p || eval i q
eval i (A p q)  =  eval i p && eval i q

interps :: [Char] -> [[Char]]
interps  =  subsequences

{-
cnf :: Prop -> Prop
cnf p  =  disin (negin p)
-}

negin :: Prop -> Prop
negin = observe "negin" negin'
negin' (V v)        =  V v
negin' (N (V v))    =  N (V v)
negin' (N (N p))    =  negin p
negin' (N (O p q))  =  O (negin (N p)) (negin (N q))
negin' (N (A p q))  =  A (negin (N p)) (negin (N q))
negin' (O p q)      =  O (negin p) (negin q)
negin' (A p q)      =  A (negin p) (negin q)
-- negin' p            =  p

prop_negin_complete :: Prop -> Bool
prop_negin_complete p  =  not (anywhere negout (negin p))
  where
  negout (N (V _))  =  False
  negout (N _)      =  True
  negout _          =  False

prop_negin_sound :: Prop -> Bool
prop_negin_sound p  =  equiv (negin p) p

eg :: Prop
eg  =  N (O (A (V 'p') (N (V 'q'))) (V 'r'))

eg' :: Prop
eg' = V 'p'

prop_negin_correct p = prop_negin_sound p && prop_negin_complete p

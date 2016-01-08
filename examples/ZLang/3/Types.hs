{-# LANGUAGE DeriveGeneric #-}
module Types where
import qualified Data.List as List
import GHC.Generics
import Debug.Hoed.Pure hiding (Forall)
import qualified Data.Functor.Rep as Rep
import qualified Data.Map as Map

data Type
  = IntType
  | BoolType
  | StringType
  | RealType

  | Name String [Type]
  | Array Type
  | Tuple [Type]
  | Record Bool [(String, Type)]
  | Forall Int Type
  | Arrow Type Type
  | Union Type Type
  | Intersect Type Type
  | TypeVar Int
  | Error
    deriving (Eq, Ord, Show, Generic)
instance Observable Type

instance (Show k, Show v) => Observable (Map.Map k v) where
  observer m = send ("Map.fromList " ++ show (Map.toList m)) (return m)

{-instance Show Type where
  show IntType = "Int"
  show BoolType = "Bool"
  show StringType = "String"
  show RealType = "Real"

  show (Union t1 (Union t2 t3)) = show t1 ++ " | " ++ "(" ++ show t2 ++ " | " ++ show t3 ++ ")"
  show (Union t1 t2) = show t1 ++ " | " ++ show t2

  show (Intersect t1 (Intersect t2 t3)) = show t1 ++ " & (" ++ show t2 ++ " & " ++ show t3 ++ ")"

  show (Intersect t1 (Union t2 t3)) = show t1 ++ " & (" ++ show t2 ++ " | " ++ show t3 ++ ")"
  show (Intersect (Union t1 t2) t3) = "(" ++ show t1 ++ " | " ++ show t2 ++ ") & " ++ show t1
  show (Intersect t1 t2) = show t1 ++ " & " ++ show t2

  show (Arrow t1 (Union t2 t3)) = show t1 ++ " -> (" ++ show t2 ++ " | " ++ show t3 ++ ")"
  show (Arrow (Union t1 t2) t3) = "(" ++ show t1 ++ " | " ++ show t2 ++ ") -> " ++ show t3 ++ ")"
  show (Arrow (Arrow t1 t2) t3) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")" ++ " -> " ++ show t3
  show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2

  show (Name s []) = s
  show (Name s tys) = s ++ "<" ++ (List.intercalate ", " (List.map show tys)) ++ ">"
  show (Array t) = "[" ++ show t ++ "]"
  show (Tuple tys) = "(" ++ List.intercalate "," (List.map show tys) ++ ")"
  show (Record _ fields) = "{" ++ List.intercalate ", " (List.map showField fields) ++ "}"
    where showField (s, t) = s ++ ": " ++ show t
  show (Forall u ty) = "forall " ++ show u ++ ". " ++ show ty
  show (TypeVar u) = show u
  show Error = "Error"
-}
  
-------------------------------------------------------
-- Type simplications
-------------------------------------------------------
contains :: Type -> Type -> Bool
contains IntType IntType = True
contains BoolType BoolType = True
contains StringType StringType = True
contains RealType RealType = True
contains (Union t1 t2) t3
  | t1 == t3 = True
  | t2 == t3 = True
  | otherwise = contains t1 t3 || contains t1 t3
contains (Intersect t1 t2) t3
  | t1 == t3 && t2 == t3 = True
  | otherwise = contains t1 t3 && contains t1 t3
contains (Arrow t1 t2) (Arrow t3 t4) =
  t1 == t3 && t2 == t4
contains (Name s1 tys1) (Name s2 tys2) =
  s1 == s2 && tys1 == tys2
contains (Array t1) (Array t2) =
  t1 == t2
contains (Tuple tys1) (Tuple tys2)
  | List.length tys1 == List.length tys2 =
     List.all (uncurry contains) (List.zip tys1 tys2)
  | otherwise = False
contains (Record _ fields1) (Record _ fields2)
  | List.length fields1 == List.length fields2 =
    List.all (uncurry (==)) (List.zip fields1 fields2)
  | otherwise = False
contains (Forall u1 ty1) (Forall u2 ty2)
  | u1 == u2 && ty1 == ty2 = True
  | otherwise = False
contains (Forall u t1) t2 = contains t1 t2
contains (TypeVar u1) (TypeVar u2) =
  u1 == u2
contains Error Error = True
contains _ _ = False

union :: Type -> Type -> Type
union t1 (Union t2 t3) = union (union t1 t2) t3
union t1 t2
  | t1 `contains` t2 = t1
  | otherwise = Union t1 t2

intersect :: Type -> Type -> Type
intersect t1 (Intersect t2 t3) = intersect (intersect t1 t2) t3
intersect t1 t2
  | t1 `contains` t2 = t2
  | otherwise = Intersect t1 t2

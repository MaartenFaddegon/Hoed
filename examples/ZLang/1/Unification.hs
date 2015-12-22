{-# LANGUAGE LambdaCase #-}

module Unification where
import Control.Monad
import Data.Foldable
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Set (Set)
import Data.Map (Map)
import Data.Map ((!))
import Types
import Ast
import TypedAst
import TypeUtils

makeIntersect :: Env -> ArgOrd -> Substitution -> Type -> Type -> IO (Type, Substitution)
makeIntersect env argOrd subst t1 t2 = do
  u <- mkTypeVar
  (_, subst') <- unify u (intersect t1 t2) env argOrd subst
  return (u, subst')

makeRecord :: Env -> ArgOrd -> Substitution -> Bool -> [(String, Type)] -> IO (Type, Substitution)
makeRecord env argOrd subst b fields = do
  u <- mkTypeVar
  (_, subst') <- unify u (Record b fields) env argOrd subst
  return (u, subst')

unifyTypes :: [Type] -> Env -> ArgOrd -> Substitution -> IO (Type, Substitution)
unifyTypes types env argOrd subst = do
  t <- mkTypeVar
  foldM f (t, subst) types
  where f (ty, subst) ty' =
          unify ty ty' env argOrd subst
          
inserts :: Ord a => Set a -> [a] -> Set a
inserts = List.foldr Set.insert

unify :: Type -> Type -> Env -> ArgOrd -> Substitution -> IO (Type, Substitution)
unify t1 t2 env argOrd subst =
  let
    lookup bind env s = Map.findWithDefault (bind ! s) s env

    uni trace bind1 bind2 (TypeVar u) (TypeVar u') subst =
      case (follow subst (TypeVar u), follow subst (TypeVar u')) of
        (TypeVar u, TypeVar u') ->
          return (TypeVar u', Map.insert u (TypeVar u') subst)
        (TypeVar u, t) ->
          return (t, Map.insert u t subst)
        (t, TypeVar u') ->
          return (t, Map.insert u' t subst)
        (t, t') ->
          uni trace bind1 bind2 t t' subst
    uni trace bind1 bind2 (Forall u t1) t2 subst = do
      (ty, subst') <- uni trace bind1 bind2 t1 t2 subst
      return (Forall u ty, subst')
    uni trace bind1 bind2 t1 (Forall u t2) subst =
      uni trace bind1 bind2 (Forall u t2) t1 subst
    uni trace bind1 bind2 (TypeVar u) t subst =
      case follow subst (TypeVar u) of
        TypeVar u -> return (t, Map.insert u t subst)
        t'        -> do (t'', subst') <- uni trace bind1 bind2 t t' subst
                        return (t'', Map.insert u t'' subst')
    uni trace bind1 bind2 t (TypeVar u) subst = uni trace bind1 bind2 (TypeVar u) t subst
    uni trace bind1 bind2 t1@(Name s1 types1) t2@(Name s2 types2) subst
      | Set.member s1 trace && Set.member s2 trace =
        return (union (instansiates t1 bind1') (instansiates t2 bind2'), subst)
      | Set.member s1 trace =
        uni (Set.insert s2 trace) bind1 bind2' t1 t2' subst
      | Set.member s2 trace =
        uni (Set.insert s1 trace) bind1' bind2 t1' t2 subst
      | otherwise =
        uni (inserts trace [s1, s2]) bind1' bind2' t1' t2' subst
      where t1' = lookup bind1 env s1
            t2' = lookup bind2 env s2
            bind1' = makeBindings argOrd s1 types1
            bind2' = makeBindings argOrd s2 types2
    uni trace bind1 bind2 t1@(Name s types) t2 subst
      | Set.member s trace =
        return (union (instansiates t1 bind) (instansiates t2 bind2), subst)
      | otherwise =
        uni (Set.insert s trace) bind bind2 t t2 subst
      where t = lookup bind1 env s
            bind = makeBindings argOrd s types
    uni trace bind1 bind2 t1 (Name s types) subst =
      uni trace bind1 bind2 (Name s types) t1 subst
    uni trace bind1 bind2 (Array t1) (Array t2) subst = do
      (t, subst') <- uni trace bind1 bind2 t1 t2 subst
      return (Array t, subst')
    uni trace bind1 bind2 (Tuple [t1]) (Tuple [t2]) subst = do
      (t, subst') <- uni trace bind1 bind2 t1 t2 subst
      return (Tuple [t], subst')
    uni trace bind1 bind2 (Tuple [t1]) t2 subst = do
      (t, subst') <- uni trace bind1 bind2 t1 t2 subst
      return (t, subst')
    uni trace bind1 bind2 t1 (Tuple [t2]) subst =
      uni trace bind1 bind2 (Tuple [t2]) t1 subst
    uni trace bind1 bind2 (Tuple types1) (Tuple types2) subst =
      if List.length types1 == List.length types2 then do
        (types, subst') <- unifyPairwise trace bind1 bind2 types1 types2 subst
        return (Tuple types, subst')
      else return (union (Tuple types1) (Tuple types2), subst)
    uni trace bind1 bind2 (Record b1 fields1) (Record b2 fields2) subst = do
      (types, subst') <- unifyPairwise trace bind1 bind2 types1 types2 subst
      let fields = List.zip names1 types
      if names1 == names2 then
        return (Record (b1 && b2) fields, subst')
      else return (union (Record b1 fields1) (Record b2 fields2), subst)
      where fields1' = List.sortBy (comparing fst) fields1
            fields2' = List.sortBy (comparing fst) fields2
            (names1, types1) = List.unzip fields1'
            (names2, types2) = List.unzip fields2'
    uni trace bind1 bind2 (Arrow tyDom1 tyCod1) (Arrow tyDom2 tyCod2) subst = do
      (tyDom, subst') <- uni trace bind1 bind2 tyDom1 tyDom2 subst
      (tyCod, subst'') <- uni trace bind1 bind2 tyCod1 tyCod2 subst'
      return (Arrow tyDom tyCod, subst'')
    uni trace bind1 bind2 (Union t1 t2) (Union t3 t4) subst = do
      (t13, subst') <- uni trace bind1 bind2 t1 t3 subst
      (t24, subst'') <- uni trace bind1 bind2 t2 t4 subst'
      return (union t13 t24, subst'')
    uni trace bind1 bind2 (Intersect t1 t2) (Intersect t3 t4) subst =
      let intersection = Set.intersection (Set.fromList [t1, t2]) (Set.fromList [t3, t4])
      in if Set.null intersection then
           return (union (Intersect t1 t2) (Intersect t3 t4), subst)
         else return (List.foldl1' Intersect (Set.toList intersection), subst)
    uni trace bind1 bind2 (Intersect t1 t2) t subst =
      if Set.member t (Set.fromList [t1, t2]) then
        return (t, subst)
      else return (union (Intersect t1 t2) t, subst)
    uni trace bind1 bind2 t (Intersect t1 t2) subst =
      uni trace bind1 bind2 (Intersect t1 t2) t subst
    uni trace bind1 bind2 IntType IntType subst = return (IntType, subst)
    uni trace bind1 bind2 RealType RealType subst = return (RealType, subst)
    uni trace bind1 bind2 StringType StringType subst = return (StringType, subst)
    uni trace bind1 bind2 BoolType BoolType subst = return (BoolType, subst)
    uni trace bind1 bind2 t1 t2 subst = return (union t1 t2, subst)

    unifyPairwise trace bind1 bind2 types1 types2 subst = do
      let types = List.zip types1 types2
      let f (types, subst) (t1, t2) =
            do (t, subst') <- uni trace bind1 bind2 t1 t2 subst
               return (t : types, subst')
      (types', subst') <- foldM f ([], subst) types
      return (List.reverse types', subst')

  in uni Set.empty Map.empty Map.empty t1 t2 subst

unify' :: Type -> Type -> Env -> ArgOrd -> Substitution -> IO (Maybe (Type, Substitution))
unify' t1 t2 env argOrd subst =
  let
    lookup bind env s = Map.findWithDefault (bind ! s) s env

    uni' trace bind1 bind2 (TypeVar u) (TypeVar u') subst = do
      case (follow subst (TypeVar u), follow subst (TypeVar u')) of
        (TypeVar u, TypeVar u') -> return $ Just (TypeVar u', Map.insert u (TypeVar u') subst)
        (TypeVar u, t) -> return $ Just (t, Map.insert u t subst)
        (t, TypeVar u') -> return $ Just (t, Map.insert u' t subst)
        (t, t') -> uni' trace bind1 bind2 t t' subst
    uni' trace bind1 bind2 (Forall u t1) t2 subst = do
      uni' trace bind1 bind2 t1 t2 subst >>= \case
        Just (ty, subst') -> return $ Just (Forall u ty, subst')
        Nothing -> return Nothing
    uni' trace bind1 bind2 t1 (Forall u t2) subst =
      uni' trace bind1 bind2 (Forall u t2) t1 subst
    uni' trace bind1 bind2 (TypeVar u) t subst =
      case follow subst (TypeVar u) of
        TypeVar u -> return $ Just (t, Map.insert u t subst)
        t' -> uni' trace bind1 bind2 t' t subst >>= \case
                Just (t'', subst') ->
                  return $ Just (t'', Map.insert u t'' subst')
                Nothing -> return Nothing
    uni' trace bind1 bind2 t (TypeVar u) subst =
      uni' trace bind1 bind2 (TypeVar u) t subst
    uni' trace bind1 bind2 t1@(Name s1 types1) t2@(Name s2 types2) subst
      | Set.member s1 trace && Set.member s2 trace =
        return Nothing
      | Set.member s1 trace =
        uni' (Set.insert s2 trace) bind1 bind2' t1 t2' subst
      | Set.member s2 trace =
        uni' (Set.insert s1 trace) bind1' bind2 t1' t2 subst
      | otherwise =
        uni' (inserts trace [s1, s2]) bind1' bind2' t1' t2' subst
      where t1' = lookup bind1 env s1
            t2' = lookup bind2 env s2
            bind1' = makeBindings argOrd s1 types1
            bind2' = makeBindings argOrd s2 types2
    uni' trace bind1 bind2 (Name s types) t2 subst
      | Set.member s trace =
        return Nothing
      | otherwise =
        uni' (Set.insert s trace) bind bind2 t t2 subst
      where t = lookup bind1 env s
            bind = makeBindings argOrd s types
    uni' trace bind1 bind2 t1 (Name s types) subst =
      uni' trace bind1 bind2 (Name s types) t1 subst
    uni' trace bind1 bind2 (Array t1) (Array t2) subst =
      uni' trace bind1 bind2 t1 t2 subst >>= \case
        Just (t, subst') -> return $ Just (Array t, subst')
        Nothing -> return Nothing
    uni' trace bind1 bind2 (Tuple [t1]) (Tuple [t2]) subst =
      uni' trace bind1 bind2 t1 t2 subst
    uni' trace bind1 bind2 (Tuple [t1]) t2 subst =
      uni' trace bind1 bind2 t1 t2 subst
    uni' trace bind1 bind2 t1 (Tuple [t2]) subst =
      uni' trace bind1 bind2 t1 t2 subst
    uni' trace bind1 bind2 (Tuple types1) (Tuple types2) subst = do
      unifyPairwise' trace bind1 bind2 types1 types2 subst >>= \case
        Just (types, subst') -> return $ Just (Tuple types, subst')
        Nothing -> return Nothing
    uni' trace bind1 bind2 (Record b1 fields1) (Record b2 fields2) subst
      | names1 == names2 = do
        unifyPairwise' trace bind1 bind2 types1 types2 subst >>= \case
          Just (types, subst') ->
            let fields = List.zip names1 types
            in return $ Just (Record (b1 && b2) fields, subst')
          Nothing -> return Nothing
      | otherwise = return Nothing
      where fields1' = List.sortBy (comparing fst) fields1
            fields2' = List.sortBy (comparing fst) fields2
            (names1, types1) = List.unzip fields1'
            (names2, types2) = List.unzip fields2'
    uni' trace bind1 bind2 (Arrow tyDom1 tyCod1) (Arrow tyDom2 tyCod2) subst = do
      uni' trace bind1 bind2 tyDom2 tyDom1 subst >>= \case
        Just (tyDom, subst') -> do
          uni' trace bind1 bind2 tyCod2 tyCod1 subst' >>= \case
            Just (tyCod, subst'') -> return $ Just (Arrow tyDom tyCod, subst'')
            Nothing -> return Nothing
        Nothing -> return Nothing
    uni' trace bind1 bind2 (Union t11 t12) (Union t21 t22) subst = do
      res1121 <- uni' trace bind1 bind2 t11 t21 subst
      res1122 <- uni' trace bind1 bind2 t11 t22 subst
      case (res1121, res1122) of
        (Just (t1121, subst1121), _) -> do
          res1221 <- uni' trace bind1 bind2 t12 t21 subst1121
          res1222 <- uni' trace bind1 bind2 t12 t22 subst1121
          case (res1221, res1222) of
            (Just (t1221, subst1221), _) -> return $ Just (union t1121 t1221, subst1221)
            (_, Just (t1222, subst1222)) -> return $ Just (union t1121 t1222, subst1222)
        (_, Just (t1122, subst1122)) -> do
          res1221 <- uni' trace bind1 bind2 t12 t21 subst1122
          res1222 <- uni' trace bind1 bind2 t12 t22 subst1122
          case (res1221, res1222) of
            (Just (t1221, subst1221), _) -> return $ Just (union t1122 t1221, subst1221)
            (_, Just (t1222, subst1222)) -> return $ Just (union t1122 t1222, subst1222)
        (_, _) -> return Nothing
    uni' trace bind1 bind2 ty (Union t1 t2) subst =
      uni' trace bind1 bind2 ty t1 subst >>= \case
        Just (t, subst') -> return $ Just (Union t t2, subst')
        Nothing -> uni' trace bind1 bind2 ty t2 subst >>= \case
                     Just (t, subst') -> return $ Just (Union t1 t, subst')
                     Nothing          -> return Nothing
    uni' trace bind1 bind2 (Union t1 t2) ty subst =
      uni' trace bind1 bind2 ty (Union t1 t2) subst
    -- TODO: This shouldn't intersect using equality. Use unification instead
    uni' trace bind1 bind2 (Intersect t1 t2) (Intersect t3 t4) subst =
      let intersection = Set.intersection (Set.fromList [t1, t2]) (Set.fromList [t3, t4])
      in if Set.null intersection then return Nothing
         else return $ Just (List.foldl1' Intersect (Set.toList intersection), subst)
    uni' trace bind1 bind2 (Intersect t1 t2) t subst
      | Set.member t (Set.fromList [t1, t2]) = return $ Just (t, subst)
      | otherwise = return Nothing
    uni' trace bind1 bind2 t (Intersect t1 t2) subst =
      uni' trace bind1 bind2 (Intersect t1 t2) t subst
    uni' trace bind1 bind2 IntType IntType subst = return $ Just (IntType, subst)
    uni' trace bind1 bind2 RealType RealType subst = return $ Just (RealType, subst)
    uni' trace bind1 bind2 StringType StringType subst = return $ Just (StringType, subst)
    uni' trace bind1 bind2 BoolType BoolType subst = return $ Just (BoolType, subst)
    uni' trace _ _ _ _ _ = return Nothing

    unifyPairwise' trace bind1 bind2 types1 types2 subst =
      if List.length types1 /= List.length types2 then
        return Nothing
      else foldrM f (Just ([], subst)) (List.zip types1 types2)
      where f (t1, t2) (Just (types, subst)) =
              uni' trace bind1 bind2 t1 t2 subst >>= \case
                Just (t, subst') -> return $ Just (t : types, subst')
                Nothing -> return Nothing
            f _ Nothing = return Nothing

  in uni' Set.empty Map.empty Map.empty t1 t2 subst

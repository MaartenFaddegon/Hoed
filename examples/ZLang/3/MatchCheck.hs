{-# LANGUAGE LambdaCase, DeriveGeneric #-}
module MatchCheck where

import Prelude hiding (and)
import Types
import TypedAst
import TypeUtils
import Utils
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set(Set)
import Data.Map(Map, (!))
import Data.Maybe
import Data.Tuple
import Data.Ord
import Control.Monad.Loops
import Control.Monad.Writer.Lazy
import Control.Monad.State
import GHC.Generics
import Debug.Hoed.Pure

-- TODO: Deriving Generic for testing purposes
data CoveringResult
  = Covered
  | Nonexhaustive Pattern
  | Redundant TypedMatchExpr
  deriving (Show, Generic, Eq)
instance Observable CoveringResult
  
data Pattern = BottomPattern Type --Match nothing of given type
             | TopPattern Type --Match anything of given type
             | IntPattern Int
             | StringPattern String
             | BoolPattern Bool
             | ArrayPattern [Pattern]
             | TuplePattern [Pattern]
             | RecordPattern [(String, Pattern)]
             | UnionPattern Pattern Pattern
             | DifferencePattern Pattern Pattern
  deriving (Eq, Show, Generic)
instance Observable Pattern

{-instance Show Pattern where
  show (BottomPattern _) = "_"
  show (TopPattern _) = "_"
  show (IntPattern n) = show n
  show (StringPattern s) = "\"" ++ s ++ "\""
  show (BoolPattern b) = show b
  show (ArrayPattern ps) = "[" ++ List.intercalate ", " (List.map show ps) ++ "]"
  show (TuplePattern ps) = "(" ++ List.intercalate ", " (List.map show ps) ++ ")"
  show (RecordPattern ps) = "{" ++ List.intercalate ", " (List.map f ps) ++ "}"
    where f (s, p) = s ++ " = " ++ show p
  show (UnionPattern p@(DifferencePattern p1 p2) p3) = "(" ++ show p ++ ") | " ++ show p3
  show (UnionPattern p1 p@(DifferencePattern p2 p3)) = show p1 ++ " | (" ++ show p ++ ")"
  show (UnionPattern p1 p2) = show p1 ++ " | " ++ show p2
  show (DifferencePattern p@(UnionPattern p1 p2) p3) = "(" ++ show p ++ ")" ++ " - " ++ show p3
  show (DifferencePattern p1 p@(UnionPattern p2 p3)) = show p1 ++ " - (" ++ show p ++ ")"
  show (DifferencePattern p1 p2) = show p1 ++ " - " ++ show p2
-}

formatMatchWarning :: CoveringResult -> String
formatMatchWarning Covered = ""
formatMatchWarning (Nonexhaustive p) =
  unlines ["Warning: Match is not exhaustive.",
           "\tMissing pattern: " ++ show p]
formatMatchWarning (Redundant tmexpr) =
  unlines ["Warning: Match has redundant pattern.",
           "\tRedundant pattern: " ++ ppTypedMatchExpr tmexpr]

matchCheck :: Env -> [TypedDecl] -> Writer [CoveringResult] Bool
matchCheck env = allM (check env)

check :: Env -> TypedDecl -> Writer [CoveringResult] Bool
check env (TTypeDecl name args ty) = return True
check env (TFunDecl name tyargs tmexprs retTy stmt) =
  ands (checkStmt env stmt : List.map (checkMatchExpr env) tmexprs)

ands = List.foldr (liftM2 (&&)) (return True)

matchable :: Type -> Bool
matchable (Arrow _ _) = False
matchable RealType = False
matchable _ = True

checkStmt :: Env -> TypedStatement -> Writer [CoveringResult] Bool
checkStmt env (TIfStatement expr stmtThen Nothing) =
  ands [checkExpr env expr, checkStmt env stmtThen]
checkStmt env (TIfStatement expr stmtThen (Just stmtElse)) =
  ands [checkExpr env expr, checkStmt env stmtThen, checkStmt env stmtElse]
checkStmt env (TWhileStatement expr stmt) =
  ands [checkExpr env expr, checkStmt env stmt]
checkStmt env (TForStatement mexpr expr stmt) =
  ands [checkMatchExpr env mexpr, checkExpr env expr, checkStmt env stmt]
checkStmt env (TCompoundStatement stmts) =
  ands (List.map (checkStmt env) stmts)
checkStmt env (TAssignStatement (Left mexpr) expr) =
  ands [checkMatchExpr env mexpr, checkExpr env expr]
checkStmt env (TAssignStatement (Right lvexpr) expr) =
  ands [checkLvalExpr env lvexpr, checkExpr env expr]
checkStmt env (TMatchStatement (expr, ty) actions)
  | matchable ty = do
    tell (covering env (ideal env ty) (List.map fst actions))
    return True
  | otherwise =
    return True
checkStmt env (TReturnStatement expr) = checkExpr env expr
checkStmt _ TBreakStatement = return True
checkStmt _ TContinueStatement = return True
checkStmt env (TDeclStatement decl) = check env decl

checkExpr :: Env -> TypedExpr -> Writer [CoveringResult] Bool
checkExpr env (TIntExpr n, t) = return True
checkExpr env (TRealExpr d, t) = return True
checkExpr env (TBoolExpr b, t) = return True
checkExpr env (TStringExpr s, t) = return True
checkExpr env (TOrExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TAndExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TEqExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TNeqExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TLtExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TGtExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TLeExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TGeExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TAddExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TSubExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TMultExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TDivExpr expr1 expr2, t) =
  ands [checkExpr env expr1, checkExpr env expr2]
checkExpr env (TUnaryMinusExpr expr, t) =
  checkExpr env expr
checkExpr env (TBangExpr expr, t) = checkExpr env expr
checkExpr env (TCallExpr funexpr argexpr, t) =
  ands [checkExpr env funexpr, checkExpr env argexpr]
checkExpr env (TListExpr exprs, t) =
  ands (List.map (checkExpr env) exprs)
checkExpr env (TTupleExpr exprs, t) =
  ands (List.map (checkExpr env) exprs)
checkExpr env (TRecordExpr fields, t) =
  ands (List.map ((checkExpr env) . snd) fields)
checkExpr env (TLValue lvexpr, t) =
  checkLvalExpr env lvexpr
checkExpr env (TLambdaExpr mexprs stmt, t) =
  ands (checkStmt env stmt : (List.map (checkMatchExpr env) mexprs))

checkMatchExpr :: Env -> TypedMatchExpr -> Writer [CoveringResult] Bool
checkMatchExpr env (mexpr, ty)
  | matchable ty = do
    tell (covering env (ideal env ty) [(mexpr, ty)])
    return True
  | otherwise =
    return True

checkLvalExpr :: Env -> TypedLValueExpr -> Writer [CoveringResult] Bool
checkLvalExpr env (TVarExpr s, t) = return True
checkLvalExpr env (TFieldAccessExpr lvalue s, t) = return True
checkLvalExpr env (TArrayAccessExpr lvalue expr, t) = checkExpr env expr

ideal :: Env -> Type -> Pattern
ideal env t = evalState (ideal' t) Set.empty
  where
    ideal' :: Type -> State (Set String) Pattern
    ideal' IntType = return $ TopPattern IntType
    ideal' BoolType = return $ TopPattern BoolType
    ideal' StringType = return $ TopPattern StringType
    ideal' (Name s tys) =
      gets (Set.member s) >>= \case
        True -> return (TopPattern (Name s tys)) --TODO: This might be bad?
        False -> modify (Set.insert s) >> ideal' (env ! s) -- TODO: missing type args
    ideal' (Array ty) = return $ TopPattern (Array ty)
    ideal' (Tuple tys) = mapM ideal' tys >>= return . TuplePattern
    ideal' (Record _ fields) =
      mapM (\(s, ty) -> ideal' ty >>=
        return . ((,) s)) fields >>= return . RecordPattern
    ideal' (Forall _ ty) = error "NYI: Forall"
    ideal' (Union ty1 ty2) = do
      p1 <- ideal' ty1
      p2 <- ideal' ty2
      return $ unionPattern p1 p2
    ideal' (Intersect ty1 ty2) = error "NYI: Intersect"
    ideal' (TypeVar u) = error "NYI: TypeVar"

unionPattern :: Pattern -> Pattern -> Pattern
unionPattern (BottomPattern IntType) p@(IntPattern n) = p
unionPattern (BottomPattern StringType) p@(StringPattern s) = p
unionPattern (BottomPattern BoolType) p@(BoolPattern b) = p
unionPattern (BottomPattern (Array ty)) p@(ArrayPattern ps) =
  ArrayPattern (List.map (unionPattern (BottomPattern ty)) ps)
unionPattern (BottomPattern (Tuple tys)) p@(TuplePattern ps)
  | List.length tys == List.length ps =
    TuplePattern (List.zipWith unionPattern (List.map BottomPattern tys) ps)
unionPattern (BottomPattern (Record _ fieldst)) (RecordPattern fieldsp)
  | subdomain mapt mapp =
    RecordPattern (List.map unionIfPresent fieldsp)
  where subdomain = Map.isSubmapOfBy (const (const True))
        mapt = Map.fromList fieldst
        mapp = Map.fromList fieldsp

        unionIfPresent (s, p) =
          case Map.lookup s mapt of
            Just t -> (s, unionPattern (BottomPattern t) p)
            Nothing -> (s, p)
unionPattern (BottomPattern ty1) p@(TopPattern ty2)
  | ty1 == ty2 = p
unionPattern (IntPattern n) p@(TopPattern IntType) = p
unionPattern (StringPattern s) p@(TopPattern StringType) = p
unionPattern (BoolPattern b) p@(TopPattern BoolType) = p
unionPattern (ArrayPattern ps) p@(TopPattern (Array ty))
  | List.all (isTop . unionPattern (TopPattern ty)) ps = p
unionPattern (TuplePattern ps) p@(TopPattern (Tuple tys))
  | List.length tys == List.length ps &&
    List.all isTop (List.zipWith unionPattern (List.map TopPattern tys) ps) = p
unionPattern (RecordPattern fieldsp) p@(TopPattern (Record _ fieldst))
  | subdomain mapt mapp &&
    List.all isTop (List.map (snd . unionIfPresent) fieldsp) = p
  where subdomain = Map.isSubmapOfBy (const (const True))
        mapt = Map.fromList fieldst
        mapp = Map.fromList fieldsp
        
        unionIfPresent (s, p) =
          case Map.lookup s mapt of
            Just t -> (s, unionPattern (TopPattern t) p)
            Nothing -> (s, p)
unionPattern p@(IntPattern n1) (IntPattern n2)
  | n1 == n2 = p
unionPattern p@(StringPattern s1) (StringPattern s2)
  | s1 == s2 = p
unionPattern p@(BoolPattern b1) (BoolPattern b2)
  | b1 == b2 = p
unionPattern (ArrayPattern ps1) (ArrayPattern ps2)
  | List.length ps1 == List.length ps2 =
    ArrayPattern (List.zipWith unionPattern ps1 ps2)
unionPattern (TuplePattern ps1) (TuplePattern ps2)
  | List.length ps1 == List.length ps2 =
    TuplePattern (List.zipWith unionPattern ps1 ps2)
unionPattern (RecordPattern fields1) (RecordPattern fields2)
  | Map.keysSet map1 == Map.keysSet map2 =
    RecordPattern (Map.toList $ Map.unionWith unionPattern map1 map2)
  where map1 = Map.fromList fields1
        map2 = Map.fromList fields2
unionPattern (UnionPattern p1 p2) (UnionPattern p3 p4) =
  UnionPattern (UnionPattern (UnionPattern p1 p2) p3) p4
unionPattern p1 (UnionPattern p2 p3) = UnionPattern (UnionPattern p1 p2) p3
unionPattern p1 p2 = UnionPattern p1 p2

differencePattern = observe "differencePattern" differencePattern'

differencePattern' :: Pattern -> Pattern -> Pattern
differencePattern' (UnionPattern p1 p2) p3
  | p1 == p3 = p2
  | p2 == p3 = p1
differencePattern' p1 p2 = DifferencePattern p1 p2

isBottom :: Pattern -> Bool
isBottom (BottomPattern _) = True
isBottom (ArrayPattern ps) = List.all isBottom ps
isBottom (TuplePattern ps) = List.all isBottom ps
isBottom (UnionPattern p1 p2) = isBottom p1 && isBottom p2
isBottom (DifferencePattern p1 p2) = instanceOf List.and p1 p2
isBottom (RecordPattern fields) = List.all isBottom (List.map snd fields)
isBottom _ = False

isTop :: Pattern -> Bool
isTop (TopPattern _) = True
isTop (ArrayPattern ps) = List.all isTop ps
isTop (TuplePattern ps) = List.all isTop ps
isTop (UnionPattern p1 p2) = isTop p1 && isTop p2
isTop (DifferencePattern p1 p2) = instanceOf List.and p1 p2
isTop _ = False

refine' :: Pattern -> Pattern -> Pattern
refine' p q = ref p q
  where
    ref :: Pattern -> Pattern -> Pattern
    ref = observe "ref" ref'
    ref' p (BottomPattern _) = p
    ref' (BottomPattern ty) _ = BottomPattern ty
    ref' (TopPattern ty1) (TopPattern ty2)
      | ty1 == ty2 = BottomPattern ty1
      | otherwise  = differencePattern (TopPattern ty1) (TopPattern ty2)
    ref' p1 (UnionPattern p2 p3) = ref (ref p1 p2) p3
    ref' p1 (DifferencePattern p2 p3) = ref p1 (ref p2 p3)
    ref' (IntPattern _) (TopPattern IntType) = BottomPattern IntType
    ref' (IntPattern n1) (IntPattern n2)
      | n1 == n2  = BottomPattern IntType
      | otherwise = IntPattern n1
    ref' (StringPattern _) (TopPattern StringType) = BottomPattern StringType
    ref' (StringPattern s1) (StringPattern s2)
      | s1 == s2  = BottomPattern StringType
      | otherwise = StringPattern s1
    ref' (BoolPattern _) (TopPattern BoolType) = BottomPattern BoolType
    ref' (BoolPattern b1) (BoolPattern b2)
      | b1 == b2  = BottomPattern BoolType
      | otherwise = BoolPattern b1
    ref' (ArrayPattern ps1) (ArrayPattern ps2)
      | n1 > n2   = ArrayPattern (zipWith ref ps1 ps2 ++ List.drop n2 ps1)
      | n1 < n2   = ArrayPattern (zipWith ref ps1 ps2 ++ List.drop n1 ps2)
      | otherwise = ArrayPattern (zipWith ref ps1 ps2)
      where n1 = List.length ps1
            n2 = List.length ps2
    ref' (ArrayPattern ps) (TopPattern (Tuple tys))
      | List.length ps == List.length tys =
        ArrayPattern (zipWith ref ps (List.map TopPattern tys))
    ref' (TuplePattern [p1]) p2 = ref p1 p2
    ref' p1 (TuplePattern [p2]) = ref p1 p2
    ref' (TuplePattern ps1) (TuplePattern ps2)
      | n1 > n2   = TuplePattern (zipWith ref ps1 ps2 ++ List.drop n2 ps1)
      | n1 < n2   = TuplePattern (zipWith ref ps1 ps2 ++ List.drop n1 ps2)
      | otherwise = TuplePattern (zipWith ref ps1 ps2)
      where n1 = List.length ps1
            n2 = List.length ps2
    ref' (RecordPattern fields1) (RecordPattern fields2)
      | List.null exclude = RecordPattern include
      | otherwise         = differencePattern (RecordPattern include) (RecordPattern exclude)
      where (include, exclude) = foldr f ([], []) fields1
            f (s, p) (include, exclude) =
              case Map.lookup s map2 of
                Just p' -> ((s, ref p p'):include, exclude)
                Nothing -> (include, (s, p):exclude)
            map2 = Map.fromList fields2
    ref' (DifferencePattern p1 p2) p3 = case ref p1 p3 of
        DifferencePattern q1 q2 -> differencePattern q1 (unionPattern p2 q2)
        q -> ref q p2
    ref' p1 p2 = differencePattern p1 p2

create :: Env -> TypedMatchExpr -> Pattern
create env (TTupleMatchExpr mexprs, _) = TuplePattern (List.map (create env) mexprs)
create env (TListMatchExpr mexprs, _) = ArrayPattern (List.map (create env) mexprs)
create env (TRecordMatchExpr fields, _) = RecordPattern (List.map f fields)
  where f (s, tmexpr) = (s, create env tmexpr)
create env (TVarMatch _, t) = ideal env t
create env (TIntMatchExpr n, _) = IntPattern n
create env (TStringMatchExpr s, _) = StringPattern s
create env (TBoolMatchExpr b, _) = BoolPattern b

instanceOf = observe "instanceOf" instanceOf'

instanceOf' :: ([Bool] -> Bool) -> Pattern -> Pattern -> Bool
instanceOf' pred _ (BottomPattern _) = False
instanceOf' pred (BottomPattern IntType) (IntPattern _) = True
instanceOf' pred (BottomPattern StringType) (StringPattern _) = True
instanceOf' pred (BottomPattern BoolType) (BoolPattern _) = True
instanceOf' pred (BottomPattern (Array ty)) (ArrayPattern ps) =
  List.all (instanceOf pred (BottomPattern ty)) ps
instanceOf' pred (BottomPattern (Tuple tys)) (TuplePattern ps)
  | List.length tys == List.length ps =
    List.all (uncurry (instanceOf pred)) (List.zip (List.map BottomPattern tys) ps)
instanceOf' pred (BottomPattern (Record _ fieldst)) (RecordPattern fieldsp) =
  Map.isSubmapOfBy (instanceOf pred) mapt mapp
  where mapp = Map.fromList fieldsp
        mapt = Map.fromList (List.map (\(s, ty) -> (s, BottomPattern ty)) fieldst)
instanceOf' pred (TopPattern ty1) (TopPattern ty2)
  | ty1 == ty2 = True
  | otherwise  = False
instanceOf' pred (TopPattern (Array ty)) (ArrayPattern ps) =
  List.all (instanceOf pred (TopPattern ty)) ps
instanceOf' pred (TopPattern (Tuple tys)) (TuplePattern ps)
  | List.length tys == List.length ps =
    List.all (uncurry (instanceOf pred)) (List.zip (List.map TopPattern tys) ps)
instanceOf' pred (TopPattern (Record _ fieldst)) (RecordPattern fieldsp)
  | List.map fst mapt == List.map fst mapp =
    List.all (uncurry (instanceOf pred)) (List.zip (List.map snd mapt) (List.map snd mapp))
  where mapt = sort (List.map (\(s, ty) -> (s, TopPattern ty)) fieldst)
        mapp = sort fieldsp
        sort = List.sortBy (comparing fst)
instanceOf' pred (IntPattern _) (TopPattern IntType) = True
instanceOf' pred (StringPattern _) (TopPattern StringType) = True
instanceOf' pred (BoolPattern _) (TopPattern BoolType) = True
instanceOf' pred (ArrayPattern ps) (TopPattern (Array ty)) =
  List.all (flip (instanceOf pred) (TopPattern ty)) ps
instanceOf' pred (TuplePattern ps) (TopPattern (Tuple tys))
  | List.length ps == List.length tys =
    List.all (uncurry (instanceOf pred)) (List.zip ps (List.map TopPattern tys))
instanceOf' pred (RecordPattern fieldsp) (TopPattern (Record _ fieldst)) =
  Map.isSubmapOfBy (instanceOf pred) mapp mapt
  where mapp = Map.fromList fieldsp
        mapt = Map.fromList (List.map (\(s, ty) -> (s, TopPattern ty)) fieldst)
instanceOf' pred (IntPattern n1) (IntPattern n2)
  | n1 == n2 = True
  | otherwise = False
instanceOf' pred (StringPattern s1) (StringPattern s2)
  | s1 == s2  = True
  | otherwise = False
instanceOf' pred (BoolPattern b1) (BoolPattern b2)
  | b1 == b2  = True
  | otherwise = False
instanceOf' pred (ArrayPattern ps1) (ArrayPattern ps2)
  | n1 == n2 = pred (List.zipWith (instanceOf pred) ps1 ps2)
  | otherwise = False
  where n1 = List.length ps1
        n2 = List.length ps2
instanceOf' pred (TuplePattern [p1]) p2 = instanceOf pred p1 p2
instanceOf' pred p1 (TuplePattern [p2]) = instanceOf pred p1 p2
instanceOf' pred (TuplePattern ps1) (TuplePattern ps2)
  | n1 == n2 = pred (List.zipWith (instanceOf pred) ps1 ps2)
  | otherwise = False
  where n1 = List.length ps1
        n2 = List.length ps2
instanceOf' pred (RecordPattern fields1) (RecordPattern fields2)
  = Map.foldWithKey f False map1
  where f s p True = True
        f s p False =
          case Map.lookup s map2 of
            Just q -> instanceOf pred p q
            Nothing -> False
        map1 = Map.fromList (List.filter (\(s, p) -> not (isBottom p)) fields1)
        map2 = Map.fromList fields2
instanceOf' pred p1 (DifferencePattern p2 p3)
  | instanceOf pred p1 p2 = not (instanceOf List.and p1 p3)
  | otherwise        = False
instanceOf' pred (UnionPattern p1 p2) p3 =
  instanceOf pred p1 p3 && instanceOf pred p2 p3
instanceOf' pred p1 (UnionPattern p2 p3) =
  instanceOf pred p1 p2 || instanceOf pred p1 p3
instanceOf' pred p1 p2 = False

refine = observe "refine" refine'

covering = observe "covering" covering'

covering' :: Env -> Pattern -> [TypedMatchExpr] -> [CoveringResult]
covering' env p []
  | isBottom p = [Covered]
  | otherwise  = [Nonexhaustive p]
covering' env p (mexpr:mexprs)
  | instanceOf List.or q p = covering env (refine p q) mexprs
  | otherwise      =
    Redundant mexpr : covering env (refine p q) mexprs
  where q = create env mexpr
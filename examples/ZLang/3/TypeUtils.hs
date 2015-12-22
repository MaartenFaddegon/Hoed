module TypeUtils where
import Data.Map (Map)
import Control.Monad
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import TypedAst
import Types
import Data.IORef
import System.IO.Unsafe

type Substitution = Map Int Type
type Env = Map String Type
type ArgOrd = Map String (Map Int String)
type Bindings = Map String Type

exprOf = fst
typeOf = snd

normaliseFields :: Ord a => [(a, b)] -> [(a, b)]
normaliseFields = List.sortBy (comparing fst)

equalRecordFields fields1 fields2 =
  let f fields = List.map fst (normaliseFields fields)
  in f fields1 == f fields2

inferList :: (a -> Env -> ArgOrd -> Substitution -> IO (b, Env, ArgOrd, Substitution)) ->
              Env -> ArgOrd -> Substitution -> [a] -> IO ([b], Env, ArgOrd, Substitution)
inferList inferer env argOrd subst list =
  do (list', env', argOrd', subst') <- foldM f ([], env, argOrd, subst) list
     return (List.reverse list', env', argOrd', subst')
  where f (list', env, argOrd, subst) elem = do
          (elem', env', argOrd', subst') <- inferer elem env argOrd subst
          return (elem' : list', env', argOrd', subst')

follow :: Substitution -> Type -> Type
follow subst = fol Set.empty
  where
    fol visited (TypeVar u)
      | Set.member u visited = TypeVar u
      | otherwise =
        case Map.lookup u subst of
          Just t -> fol (Set.insert u visited) t
          Nothing -> TypeVar u
    fol visited (Array ty) = Array (fol visited ty)
    fol visited (Tuple types) = Tuple (List.map (fol visited) types)
    fol visited (Record b fields) =
      let f (s, ty) = (s, fol visited ty)
      in Record b (List.map f fields)
    fol visited (Arrow tDom tCod) = Arrow (fol visited tDom) (fol visited tCod)
    fol visited (Union t1 t2) = union (fol visited t1) (fol visited t2)
    fol visited (Forall u ty) = Forall u (fol visited ty)
    fol visited (Intersect t1 t2) = Intersect (fol visited t1) (fol visited t2)
    fol _ t = t

free :: Type -> Substitution -> Bool
free ty subst =
  case follow subst ty of
    TypeVar _ -> True
    _         -> False
    
makeBindings :: ArgOrd -> String -> [Type] -> Bindings
makeBindings argOrd s types =
  case Map.lookup s argOrd of
    Just argOrd -> Map.fromList $ List.zip (Map.elems argOrd) types
    Nothing -> Map.empty

instansiate :: String -> Type -> Type -> Type
instansiate name ty t =
  let inst (Name s [])
        | s == name = ty
        | otherwise = Name s []
      inst (Name s tys) = Name s (List.map inst tys)
      inst (Forall u ty) = Forall u (inst ty)
      inst (Arrow tDom tCod) = Arrow (inst tDom) (inst tCod)
      inst (Union t1 t2) = union (inst t1) (inst t2)
      inst (Tuple tys) = Tuple (List.map inst tys)
      inst (Record b fields) = Record b (List.map (\(s, ty) -> (s, inst ty)) fields)
      inst (Array ty) = Array (inst ty)
      inst (TypeVar u) = TypeVar u
      inst (Intersect t1 t2) = intersect (inst t1) (inst t2)
      inst t = t
  in inst t

instansiates :: Type -> Map String Type -> Type
instansiates = Map.foldrWithKey instansiate

counter :: IORef Int
counter = unsafePerformIO $ newIORef (0 :: Int) -- unique counter 

mkTypeVar :: IO Type
mkTypeVar = do
  counter' <- readIORef counter
  modifyIORef counter (+ 1)
  return $ TypeVar counter'

makeArrow :: [Type] -> Type -> Type
makeArrow types retTy = List.foldr Arrow retTy types

typevars :: Type -> [Int]
typevars (TypeVar u) = [u]
typevars (Forall u ty) =
  if List.elem u uniques then uniques
  else u : uniques
  where uniques = typevars ty
typevars (Arrow t1 t2) = List.nub $ typevars t1 ++ typevars t2
typevars (Union t1 t2) = List.nub $ typevars t1 ++ typevars t2
typevars (Tuple tys) = List.nub $ concatMap typevars tys
typevars (Record _ fields) = List.nub $ concatMap typevars (List.map snd fields)
typevars (Array ty) = typevars ty
typevars (Intersect t1 t2) = List.nub $ typevars t1 ++ typevars t2
typevars _ = []

makeForall :: Substitution -> Type -> Type -> Type
makeForall subst ty1 ty2 =
  List.foldr make ty2 (typevars (follow subst ty1))
  where make u ty
          | List.elem u (typevars ty) = ty
          | otherwise                 = Forall u ty
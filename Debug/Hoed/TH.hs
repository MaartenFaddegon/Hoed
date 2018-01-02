{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Debug.Hoed.TH (obs) where

import           Control.Monad
import           Data.Generics.Uniplate.Data
import           Data.List                   (group, nub, sort, (\\))
import           Debug.Hoed
import           Debug.Hoed.Compat
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- | A handy TH wrapper for observing functions.
--
--   @
--   obs [d|
--     trimPat :: Exp S -> Pat S -> Pat S
--     trimPat vs = filterPat ((`Set.member` freeVars vs) . void)
--       |]
--   @
--
--   is equivalent to:
--
--   @
--   trimPat = observe "trimPat" trimPat'
--   trimPat' vs = filterPat ....
--   @
--
--   'obs' accepts multiple declarations, and all the functions
--   inside will be wrapped as above, while the rest of declarations
--   will stay unchanged. As such it can be used to observe entire modules.
--
obs :: Q [Dec] -> Q [Dec]
obs decs = do
  decs <- decs
  names <- sequence [ (n,) <$> newName(nameBase n ++ "Obs") | FunD n _ <- decs]
  fmap concat $ forM decs $ \dec ->
    case dec of
      FunD n xx -> do
        let Just n' = lookup n names
            nb = nameBase n
        newDecl <- funD n [clause [] (normalB [| observe nb $(varE n')|]) []]
        return [newDecl, FunD n' xx]
      SigD n ty | Just n' <- lookup n names -> do
        dec' <- adjustSig n ty
        return [dec']
      _ ->
        return [dec]

nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

----------------------------------------------------------
-- With a little help from Neil Mitchell's debug package

-- | List all the type variables of kind * (or do the best you can)
kindStar :: Type -> Q [Name]
-- in Q so we should be able to use 'reify' to do a better job
kindStar t = return $
    nubOrd [x | VarT x <- universe t] \\     -- find all variables
    nubOrd [x | AppT (VarT x) _ <- universe t] -- delete the "obvious" ones

-- try and shove in a "Observable a =>" if we can
adjustSig name (ForallT vars ctxt typ) = do
  vs <- kindStar typ
  return $
    SigD name $
    ForallT vars (nub $ map (addConstraint ''Observable . (:[]) . VarT) vs ++ ctxt) typ
adjustSig name other = adjustSig name $ ForallT [] [] other

adjustValD decl@ValD{} = transformBi adjustPat decl
adjustValD other       = other

adjustPat (VarP x) = ViewP (VarE 'observe `AppE` toLit x) (VarP x)
adjustPat x        = x

toLit (Name (OccName x) _) = LitE $ StringL x

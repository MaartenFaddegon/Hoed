{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Debug.Hoed.TH where

import           Control.Monad
import           Debug.Hoed
import           Language.Haskell.TH

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
      SigD n ty | Just n' <- lookup n names ->
        return [dec, SigD n' ty]
      _ ->
        return [dec]

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014
{-# LANGUAGE DeriveGeneric     #-}

module Debug.Hoed.Render
(CompStmt(..)
,StmtDetails(..)
,stmtRes
,renderCompStmts
,CDS
,eventsToCDS
,noNewlines
,sortOn
) where
import           Control.DeepSeq
import           Control.Exception        (assert)
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans.Reader
import           Data.Array               as Array
import           Data.Char                (isAlpha)
import           Data.Coerce
import           Data.Hashable
import           Data.List                (nub, sort, unfoldr)
import qualified Data.HashTable.ST.Cuckoo as H
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.MemoUgly
import           Data.Primitive.MutVar
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import           Data.Word
import           Debug.Hoed.Compat
import           Debug.Hoed.Observe
import           GHC.Exts(IsList(..))
import           GHC.Generics
import           Text.PrettyPrint.FPretty hiding (sep, (<$>), text)
import qualified Text.PrettyPrint.FPretty as FPretty
import           Text.Read


------------------------------------------------------------------------
-- The CompStmt type

-- MF TODO: naming here is a bit of a mess. Needs refactoring.
-- Indentifier refers to an identifier users can explicitely give
-- to observe'. But UID is the unique number assigned to each event.
-- The field equIdentifier is not an Identifier, but the UID of the
-- event that starts the observation. And stmtUIDs is the list of
-- UIDs of all events that form the statement.

data CompStmt = CompStmt { stmtLabel      :: !Text
                         , stmtIdentifier :: !UID
                         , stmtDetails    :: !StmtDetails
                         }
                deriving (Generic)

instance NFData CompStmt

instance Eq CompStmt where c1 == c2 = stmtIdentifier c1 == stmtIdentifier c2
instance Ord CompStmt where
  compare c1 c2 = compare (stmtIdentifier c1) (stmtIdentifier c2)

data StmtDetails
  = StmtCon { stmtCon :: Text
           ,  stmtPretty :: Text}
  | StmtLam { stmtLamArgs :: [Text]
           ,  stmtLamRes :: Text
           ,  stmtPretty :: Text}
  deriving (Generic)

instance NFData StmtDetails

stmtRes :: CompStmt -> Text
stmtRes = stmtPretty . stmtDetails

instance Show CompStmt where
  show = unpack . stmtRes
  showList eqs eq = unlines (map show eqs) ++ eq

noNewlines :: String -> String
noNewlines = noNewlines' False
noNewlines' :: Bool -> String -> String
noNewlines' _ [] = []
noNewlines' w (s:ss)
 | w       && (s == ' ' || s == '\n') =       noNewlines' True ss
 | not w && (s == ' ' || s == '\n') = ' ' : noNewlines' True ss
 | otherwise                          = s   : noNewlines' False ss

------------------------------------------------------------------------
-- Memoising pretty for perf and to partially recover sharing

prettyW :: (?statementWidth::Int) => Doc -> Text
prettyW doc = pack $ pretty ?statementWidth doc

------------------------------------------------------------------------
-- Render equations from CDS set

renderCompStmts :: (?statementWidth::Int) => CDSSet -> [CompStmt]
renderCompStmts cdss = concatMap renderCompStmt cdss

-- renderCompStmt: an observed function can be applied multiple times, each application
-- is rendered to a computation statement

renderCompStmt :: (?statementWidth::Int) => CDS -> [CompStmt]
renderCompStmt (CDSNamed name uid set) =
        let output = cdssToOutput set in
        concatMap (renderNamedTop name uid) output

renderCompStmt other = error $ show other

prettySet cds = prettySet_noid(coerce cds)

prettySet_noid :: (?statementWidth::Int) => [CDSsansUID] -> Text
prettySet_noid = memo (prettyW . renderSet . coerce)

renderNamedTop :: (?statementWidth::Int) => Text -> UID -> Output -> [CompStmt]
renderNamedTop name observeUid (OutData cds) = map f pairs
  where
    f (args, res, Just i) =
      CompStmt name i $
      (StmtLam (map prettySet args)
       (prettySet res)
       (prettyW $ renderNamedFn name (args, res)))
    f (_, cons, Nothing) =
      CompStmt name observeUid $
      (StmtCon (prettySet cons)
       (prettyW $ renderNamedCons name cons))
    pairs = (nubSorted . sortOn argAndRes) pairs'
    pairs' = findFn [cds]
    argAndRes (arg, res, _) = (arg, res)
renderNamedTop name _ other = error $ show other

-- local nub for sorted lists
nubSorted :: Eq a => [a] -> [a]
nubSorted []        = []
nubSorted (a:a':as) | a == a' = nubSorted (a' : as)
nubSorted (a:as)    = a : nubSorted as

-- %************************************************************************
-- %*                                                                   *
-- \subsection{The CDS and converting functions}
-- %*                                                                   *
-- %************************************************************************

data CDS = CDSNamed      !Text !UID    !CDSSet
         | CDSCons       !UID  !Text   ![CDSSet]
         | CDSFun        !UID  !CDSSet !CDSSet
         | CDSEntered    !UID
         | CDSTerminated !UID
         | CDSChar       !Char   -- only used internally in eventsToCDS
         | CDSString     !String -- only used internally in eventsToCDS
        deriving (Show,Eq,Ord,Generic)

instance NFData CDS

normalizeCDS :: CDS -> CDS
normalizeCDS (CDSString s) = CDSCons 0 (pack $ show s) []
normalizeCDS (CDSChar   s) = CDSCons 0 (pack $ show s) []
normalizeCDS other = other
type CDSSet = [CDS]

-- Monomorphized [Parent] for compactness
data ParentList = ParentCons !Int !Word8 ParentList | ParentNil
instance IsList ParentList where
  type Item ParentList = Parent
  toList = unfoldr (\case ParentNil -> Nothing ; ParentCons pp pc t -> Just (Parent pp pc,t))
  fromList = foldr (\(Parent pp pc) t -> ParentCons pp pc t) ParentNil

eventsToCDS :: Trace -> CDSSet
eventsToCDS pairs = getChild (-1) 0
   where

     -- res i = out_arr VG.! i
     res i = getNode'' i (change (pairs VG.! i))

     mid_arr :: V.Vector ParentList
     mid_arr = VG.unsafeAccumulate
                  (\i (Parent pp pc) -> ParentCons pp pc i)
                  (V.replicate (VG.length pairs) ParentNil)
                  ( VG.map (\(node, Event (Parent pnode pport) _) ->
                              (pnode+1, Parent node pport))
                  $ VG.filter (\(_,e) -> change e /= Enter)
                  $ VG.convert
                  $ VG.indexed pairs)

     getNode'' ::  Int -> Change -> CDS
     getNode'' node change =
       case change of
        Observe str         -> let chd = normalizeCDS <$> getChild node 0
                               in CDSNamed str (getId chd node) chd
        Enter               -> CDSEntered node
        Fun                 -> CDSFun node (normalizeCDS <$> getChild node 0)
                                           (normalizeCDS <$> getChild node 1)
        ConsChar char       -> CDSChar char
        Cons portc cons
                            -> simplifyCons node cons
                                 [ getChild node (fromIntegral n)
                                 | n <- [0::Int .. fromIntegral portc - 1]]

     getId []                 i  = i
     getId (CDSFun i _ _:_) _    = i
     getId (_:cs)             i  = getId cs i

     getChild :: Int -> Word8 -> CDSSet
     getChild pnode pport =
       [ res content
       | Parent content pport' <- toList $ mid_arr VG.! succ pnode
       , pport == pport'
       ]

simplifyCons :: UID -> Text -> [CDSSet] -> CDS
simplifyCons _ "throw" [[CDSCons _ "ErrorCall" set]]
  = CDSCons 0 "error" set
simplifyCons _ ":" [[CDSChar !ch], [CDSCons _ "[]" []]]
  = CDSString [ch]
simplifyCons _ ":" [[CDSChar !ch], [CDSString s]]
  = CDSString (ch:s)
simplifyCons uid con xx = CDSCons uid con (map (map normalizeCDS) xx)

render :: Int -> Bool -> CDS -> Doc
render i b cds = render_noid i b (coerce cds)

render_noid  :: Int -> Bool -> CDSsansUID -> Doc
render_noid = uncurry3 $ memo $ \(i, b, cds) -> render' i b (coerce cds)
  where
    uncurry3 f a b c = f (a,b,c)

render' prec par (CDSCons _ ":" [cds1,cds2]) =
        if par && not needParen
        then doc -- dont use paren (..) because we dont want a grp here!
        else paren needParen doc
   where
        doc = grp (sep <> renderSet' 5 False cds1 <> " : ") <>
              renderSet' 4 True cds2
        needParen = prec > 4
render' _prec _par (CDSCons _ "," cdss) | not (null cdss) =
        nest 2 ("(" <> foldl1 (\ a b -> a <> ", " <> b)
                            (map renderSet cdss) <>
                ")")
render' prec _par (CDSCons _ name cdss)
  | not (T.null name)
  , (not . isAlpha . T.head) name && length cdss > 1 = -- render as infix
        paren (prec /= 0)
                  (grp
                    (renderSet' 10 False (head cdss)
                     <> sep <> text name
                     <> nest 2 (foldr (<>) nil
                                 [ if null cds then nil else sep <> renderSet' 10 False cds
                                 | cds <- tail cdss
                                 ]
                              )
                    )
                  )
  | otherwise = -- render as prefix
        paren (not (null cdss) && prec /= 0)
                 ( grp
                   (text name <> nest 2 (foldr (<>) nil
                                          [ sep <> renderSet' 10 False cds
                                          | cds <- cdss
                                          ]
                                       )
                   )
                 )

{- renderSet handles the various styles of CDSSet.
 -}

renderSet :: CDSSet -> Doc
renderSet = renderSet' 0 False

renderSet' :: Int -> Bool -> CDSSet -> Doc
renderSet' _ _      [] = "_"
renderSet' prec par [cons@CDSCons {}]    = render prec par cons
renderSet' _prec _par cdss                   =
         "{ " <> foldl1 (\ a b -> a <> line <>
                                    ", " <> b)
                                    (map renderFn pairs) <>
                line <> "}"

   where
        findFn_noUIDs :: CDSSet -> [([CDSSet],CDSSet)]
        findFn_noUIDs c = map (\(a,r,_) -> (a,r)) (findFn c)
        pairs = nub (sort (findFn_noUIDs cdss))
        -- local nub for sorted lists
        nub []        = []
        nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)    = a : nub as

renderFn :: ([CDSSet],CDSSet) -> Doc
renderFn argsres = renderFn_noid (coerce argsres)

renderFn_noid :: ([[CDSsansUID]],[CDSsansUID]) -> Doc
renderFn_noid = memo $ \(coerce -> args :: [CDSSet], coerce -> res)
       -> grp  (nest 3
                ("\\ " <>
                 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b)
                       nil
                       args <> sep <>
                 "-> " <> renderSet res
                )
               )

renderNamedCons :: Text -> CDSSet -> Doc
renderNamedCons name cons
  = text name <> nest 2
     ( sep <> grp (text "= " <> renderSet cons)
     )

renderNamedFn :: Text -> ([CDSSet],CDSSet) -> Doc
renderNamedFn name (args,res)
  = text name <> nest 2
     ( sep <> foldr (\ a b -> grp (renderSet' 10 False a) <> sep <> b) nil args
       <> sep <> grp ("= " <> align(renderSet res))
     )

-- | Reconstructs functional values from a CDSSet.
--   Returns a triple containing:
--    1. The arguments, if any, or an empty list for non function values
--    2. The result
--    3. The id of the CDSFun, if a functional value.
findFn :: CDSSet -> [([CDSSet],CDSSet, Maybe UID)]
findFn = foldr findFn' []

findFn' :: CDS -> [([CDSSet], CDSSet, Maybe UID)] -> [([CDSSet], CDSSet, Maybe UID)]
findFn' (CDSFun i arg res) rest =
    case findFn res of
       [(args',res',_)] -> (arg : args', res', Just i) : rest
       _                -> ([arg], res, Just i) : rest
findFn' other rest = ([],[other], Nothing) : rest

paren :: Bool -> Doc -> Doc
paren False doc = grp doc
paren True  doc = grp ( "(" <> doc <> ")")

data Output = OutLabel Text CDSSet [Output]
            | OutData  CDS
              deriving (Eq,Ord,Show)

cdssToOutput :: CDSSet -> [Output]
cdssToOutput =  map cdsToOutput

cdsToOutput :: CDS -> Output
cdsToOutput (CDSNamed name _ cdsset)
            = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@OutLabel {} <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@CDSCons {} = OutData cons
cdsToOutput    fn@CDSFun {} = OutData fn

nil :: Doc
nil = Text.PrettyPrint.FPretty.empty
grp :: Doc -> Doc
grp = Text.PrettyPrint.FPretty.group
sep :: Doc
sep = softline  -- A space, if the following still fits on the current line, otherwise newline.
sp :: Doc
sp = " "   -- A space, always.

-- TODO fork FPretty to build on Text instead of Strings
text = FPretty.text . unpack

-- %************************************************************************
-- %*                                                                   *
-- \subsection{Custom Eq and Ord instances for CDS that gloss over UIDs
-- %*                                                                   *
-- %************************************************************************

newtype CDSsansUID = CDSsansUID CDS

instance Eq CDSsansUID where
  CDSsansUID(CDSNamed t _ xx) == CDSsansUID(CDSNamed t' _ yy) =
    t == t' && coerce xx == (coerce yy :: [CDSsansUID])
  CDSsansUID (CDSCons _ t xx) == CDSsansUID(CDSCons _ t' yy)  =
    t == t'  && coerce xx == (coerce yy :: [[CDSsansUID]])
  CDSsansUID (CDSFun _ res args) == CDSsansUID (CDSFun _ res' args') =
    (coerce res :: [CDSsansUID]) == coerce res' && coerce args == (coerce args' :: [CDSsansUID])
  CDSsansUID x == CDSsansUID y = x == y

instance Ord CDSsansUID where
  CDSsansUID (CDSNamed t _ xx) `compare` CDSsansUID (CDSNamed t' _ yy) =
    (t, coerce xx :: [CDSsansUID]) `compare` (t', coerce yy)
  CDSsansUID (CDSCons _ t xx) `compare` CDSsansUID (CDSCons _ t' yy) =
    (t, coerce xx :: [[CDSsansUID]]) `compare` (t', coerce yy)
  CDSsansUID (CDSFun _ args res) `compare` CDSsansUID (CDSFun _ args' res') =
    (coerce args :: [CDSsansUID], coerce res :: [CDSsansUID]) `compare` (coerce args', coerce res')
  CDSsansUID x `compare` CDSsansUID y = x `compare` y

instance Hashable CDSsansUID where
  s `hashWithSalt` CDSsansUID (CDSNamed t _ xx) = s `hashWithSalt` t `hashWithSalt` (coerce xx :: [CDSsansUID])
  s `hashWithSalt` CDSsansUID (CDSCons _  t xx) = s `hashWithSalt` t `hashWithSalt` (coerce xx :: [[CDSsansUID]])
  s `hashWithSalt` CDSsansUID (CDSFun _ args res) = s `hashWithSalt` (coerce args :: [CDSsansUID]) `hashWithSalt` (coerce res :: [CDSsansUID])

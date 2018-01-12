{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           Data.Array               as Array
import           Data.Char                (isAlpha)
import           Data.Indexable
import           Data.List                (nub, sort)
import           Data.Foldable
import           Data.Strict.Tuple
import           Debug.Hoed.Compat
import           Debug.Hoed.Observe
import           GHC.Generics
import           Prelude                  hiding (lookup)
import           Text.PrettyPrint.FPretty hiding (sep, (<$>))
import           Text.Read


------------------------------------------------------------------------
-- The CompStmt type

-- MF TODO: naming here is a bit of a mess. Needs refactoring.
-- Indentifier refers to an identifier users can explicitely give
-- to observe'. But UID is the unique number assigned to each event.
-- The field equIdentifier is not an Identifier, but the UID of the
-- event that starts the observation. And stmtUIDs is the list of
-- UIDs of all events that form the statement.

data CompStmt = CompStmt { stmtLabel      :: !String
                         , stmtIdentifier :: !UID
                         , stmtDetails    :: !StmtDetails
                         }
                deriving (Generic)

instance NFData CompStmt

instance Eq CompStmt where c1 == c2 = stmtIdentifier c1 == stmtIdentifier c2
instance Ord CompStmt where
  compare c1 c2 = compare (stmtIdentifier c1) (stmtIdentifier c2)

data StmtDetails
  = StmtCon { stmtCon :: !String
           ,  stmtPretty :: !String}
  | StmtLam { stmtLamArgs :: ![String]
           ,  stmtLamRes :: !String
           ,  stmtPretty :: !String}
  deriving (Generic)

instance NFData StmtDetails

stmtRes :: CompStmt -> String
stmtRes = stmtPretty . stmtDetails

instance Show CompStmt where
  show = stmtRes
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
-- Render equations from CDS set

renderCompStmts :: (?statementWidth::Int) => CDSSet -> [CompStmt]
renderCompStmts = concatMap renderCompStmt

-- renderCompStmt: an observed function can be applied multiple times, each application
-- is rendered to a computation statement

renderCompStmt :: (?statementWidth::Int) => CDS -> [CompStmt]
renderCompStmt (CDSNamed name uid set) = statements
  where statements :: [CompStmt]
        statements   = concatMap (renderNamedTop name uid) output
        output       = cdssToOutput set

renderCompStmt other = error $ show other

renderNamedTop :: (?statementWidth::Int) => String -> UID -> Output -> [CompStmt]
renderNamedTop name observeUid (OutData cds) = map f pairs
  where
    f (args, res, Just i) =
      CompStmt name i $
      StmtLam
        (map (prettyW . renderSet) args)
        (prettyW $ renderSet res)
        (prettyW $ renderNamedFn name (args, res))
    f (_, cons, Nothing) =
      CompStmt name observeUid $
      StmtCon (prettyW $ renderSet cons)
              (prettyW $ renderNamedCons name cons)
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

data CDS = CDSNamed      !String !UID !CDSSet
         | CDSCons       !UID    !String   ![CDSSet]
         | CDSFun        !UID              !CDSSet !CDSSet
         | CDSEntered    !UID
         | CDSTerminated !UID
         | CDSString     !String -- only used internally in eventsToCDS
        deriving (Show,Eq,Ord,Generic)

instance NFData CDS

normalizeCDS :: CDS -> CDS
normalizeCDS (CDSString s) = CDSCons 0 (show s) []
normalizeCDS other = other
type CDSSet = [CDS]

eventsToCDS :: Trace -> CDSSet
eventsToCDS pairs = getChild 0 0 
   where

     res = indexableAt out_arr . pred

     bnds = (0, length pairs)

     cons !t !h = h : t

     mid_arr :: Array Int [Pair Int CDS]
     mid_arr = accumArray cons [] bnds
                [ (pnode, (pport :!: res node))
                | (Event node (Parent pnode pport) change) <- toList pairs
                , change /= Enter
                ]

     out_arr = fmap (\e -> getNode'' (eventUID e) e (change e)) pairs

     getNode'' ::  Int -> Event -> Change -> CDS
     getNode'' node _e change =
       case change of
        Observe str         -> let chd = normalizeCDS <$> getChild node 0
                               in CDSNamed str (getId chd node) chd
        Enter               -> CDSEntered node
        Fun                 -> CDSFun node (normalizeCDS <$> getChild node 0)
                                           (normalizeCDS <$> getChild node 1)
        (Cons portc cons)
                            -> simplifyCons node cons
                                 [getChild node n | n <- [0 .. portc - 1]]

     getId []                 i  = i
     getId (CDSFun i _ _:_) _    = i
     getId (_:cs)             i  = getId cs i

     getChild :: Int -> Int -> CDSSet
     getChild pnode pport =
       [ content
       | pport' :!: content <- (!) mid_arr pnode
       , pport == pport'
       ]

simplifyCons :: UID -> String -> [CDSSet] -> CDS
simplifyCons _ "throw" [[CDSCons _ "ErrorCall" set]]
  = CDSCons 0 "error" set
simplifyCons _ ":" [[CDSCons _ (matchChar -> Just !ch) []], [CDSCons _ "[]" []]]
  = CDSString [ch]
simplifyCons _ ":" [[CDSCons _ (matchChar -> Just !ch) []], [CDSString s]]
  = CDSString (ch:s)
simplifyCons uid con xx = CDSCons uid con (map (map normalizeCDS) xx)

matchChar :: [Char] -> Maybe Char
matchChar ['\'', ch ,'\''] = Just ch
matchChar special@['\'', _, _ ,'\''] = readMaybe special
matchChar _ = Nothing

render  :: Int -> Bool -> CDS -> Doc
render prec par (CDSCons _ ":" [cds1,cds2]) =
        if par && not needParen
        then doc -- dont use paren (..) because we dont want a grp here!
        else paren needParen doc
   where
        doc = grp (sep <> renderSet' 5 False cds1 <> " : ") <>
              renderSet' 4 True cds2
        needParen = prec > 4
render _prec _par (CDSCons _ "," cdss) | not (null cdss) =
        nest 2 ("(" <> foldl1 (\ a b -> a <> ", " <> b)
                            (map renderSet cdss) <>
                ")")
render prec _par (CDSCons _ name cdss)
  | _:_ <- name
  , (not . isAlpha . head) name && length cdss > 1 = -- render as infix
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
renderFn (args, res)
        = grp  (nest 3
                ("\\ " <>
                 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b)
                       nil
                       args <> sep <>
                 "-> " <> renderSet res
                )
               )

renderNamedCons :: String -> CDSSet -> Doc
renderNamedCons name cons
  = text name <> nest 2
     ( sep <> grp (text "= " <> renderSet cons)
     )

renderNamedFn :: String -> ([CDSSet],CDSSet) -> Doc
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

data Output = OutLabel String CDSSet [Output]
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

prettyW :: (?statementWidth::Int) => Doc -> String
prettyW = pretty ?statementWidth

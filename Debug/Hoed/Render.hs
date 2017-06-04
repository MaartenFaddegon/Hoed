-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014
{-# LANGUAGE CPP, DeriveGeneric #-}

module Debug.Hoed.Render
(CompStmt(..)
,renderCompStmts
,CDS
,eventsToCDS
,rmEntrySet
,simplifyCDSSet
,noNewlines
) where
import Debug.Hoed.EventForest

import Text.PrettyPrint.FPretty hiding (sep)
import Prelude hiding(lookup)
import Debug.Hoed.Observe
import Data.List(sort,sortBy,partition,nub
#if __GLASGOW_HASKELL__ >= 710
                , sortOn
#endif
                )
import Data.Graph.Libgraph
import Data.Array as Array
import Data.Char(isAlpha)
import GHC.Generics

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))
#endif

------------------------------------------------------------------------
-- The CompStmt type

-- MF TODO: naming here is a bit of a mess. Needs refactoring.
-- Indentifier refers to an identifier users can explicitely give
-- to observe'. But UID is the unique number assigned to each event.
-- The field equIdentifier is not an Identifier, but the UID of the
-- event that starts the observation. And stmtUIDs is the list of
-- UIDs of all events that form the statement.

data CompStmt = CompStmt { stmtLabel      :: String
                         , stmtIdentifier :: UID
                         , stmtRes        :: String
                         }
                deriving (Eq,Ord,Generic)

instance Show CompStmt where
  show = stmtRes
  showList eqs eq = unlines (map show eqs) ++ eq

noNewlines :: String -> String
noNewlines = noNewlines' False
noNewlines' _ [] = []
noNewlines' w (s:ss)
 | w       && (s == ' ' || s == '\n') =       noNewlines' True ss
 | (not w) && (s == ' ' || s == '\n') = ' ' : noNewlines' True ss
 | otherwise                          = s   : noNewlines' False ss

------------------------------------------------------------------------
-- Render equations from CDS set

statementWidth = 110 -- 110 is good for papers (maybe make this configurable from the GUI?)

renderCompStmts :: CDSSet -> [CompStmt]
renderCompStmts = foldl (\acc set -> acc ++ renderCompStmt set) []

-- renderCompStmt: an observed function can be applied multiple times, each application
-- is rendered to a computation statement

renderCompStmt :: CDS -> [CompStmt]
renderCompStmt (CDSNamed name uid set)
  = map mkStmt statements
  where statements :: [(String,UID)]
        statements   = map (\(d,i) -> (pretty statementWidth d,i)) doc
        doc          = foldl (\a b -> a ++ renderNamedTop name uid b) [] output
        output       = cdssToOutput set

        mkStmt :: (String,UID) -> CompStmt
        mkStmt (s,i) = CompStmt name i s

renderNamedTop :: String -> UID -> Output -> [(Doc,UID)]
renderNamedTop name observeUid(OutData cds)
  =  map f pairs
  where f (args,res,Just i) = (renderNamedFn name (args,res), i)
        f (_,cons,Nothing)  = (renderNamedCons name cons, observeUid)
        pairs  = (nubSorted . sortOn argAndRes) pairs'
        pairs' = findFn [cds]
        argAndRes (arg,res,_) = (arg,res)


-- local nub for sorted lists
nubSorted :: Eq a => [a] -> [a]
nubSorted []                  = []
nubSorted (a:a':as) | a == a' = nub (a' : as)
nubSorted (a:as)              = a : nub as

-- %************************************************************************
-- %*                                                                   *
-- \subsection{The CDS and converting functions}
-- %*                                                                   *
-- %************************************************************************


data CDS = CDSNamed      String UID CDSSet
         | CDSCons       UID    String   [CDSSet]
         | CDSFun        UID             CDSSet CDSSet
         | CDSEntered    UID
         | CDSTerminated UID
        deriving (Show,Eq,Ord)

type CDSSet = [CDS]

eventsToCDS :: [Event] -> CDSSet
eventsToCDS pairs = getChild 0 0
   where

     res i = (!) out_arr i

     bnds = (0, length pairs)

     mid_arr :: Array Int [(Int,CDS)]
     mid_arr = accumArray (flip (:)) [] bnds
                [ (pnode,(pport,res node))
                | (Event node (Parent pnode pport) _) <- pairs
                ]

     out_arr = array bnds       -- never uses 0 index
                [ (node,getNode'' node e change)
                | e@(Event node _ change) <- pairs
                ]

     getNode'' ::  Int -> Event -> Change -> CDS
     getNode'' node e change =
       case change of
        (Observe str i) -> let chd = getChild node 0
                               in CDSNamed str (getId chd i) chd
        (Enter)             -> CDSEntered node
        (NoEnter)           -> CDSTerminated node
        Fun                 -> CDSFun node (getChild node 0) (getChild node 1)
        (Cons portc cons)
                            -> CDSCons node cons
                                  [ getChild node n | n <- [0..(portc-1)]]

     getId []                  i = i
     getId ((CDSFun i _ _ ):_) _ = i
     getId (_:cs)              i = getId cs i

     getChild :: Int -> Int -> CDSSet
     getChild pnode pport =
        [ content
        | (pport',content) <- (!) mid_arr pnode
        , pport == pport'
        ]

render  :: Int -> Bool -> CDS -> Doc
render prec par (CDSCons _ ":" [cds1,cds2]) =
        if (par && not needParen)
        then doc -- dont use paren (..) because we dont want a grp here!
        else paren needParen doc
   where
        doc = grp (sep <> renderSet' 5 False cds1 <> text " : ") <>
              renderSet' 4 True cds2
        needParen = prec > 4
render prec par (CDSCons _ "," cdss) | length cdss > 0 =
        nest 2 (text "(" <> foldl1 (\ a b -> a <> text ", " <> b)
                            (map renderSet cdss) <>
                text ")")
render prec par (CDSCons _ name cdss)
  | (not . isAlpha . head) name && length cdss > 1 = -- render as infix
        paren (prec /= 0)
                  (grp
                    (renderSet' 10 False (head cdss)
                     <> sep <> text name
                     <> nest 2 (foldr (<>) nil
                                 [ if cds == [] then nil else sep <> renderSet' 10 False cds
                                 | cds <- tail cdss
                                 ]
                              )
                    )
                  )
  | otherwise = -- render as prefix
        paren (length cdss > 0 && prec /= 0)
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
renderSet' _ _      [] = text "_"
renderSet' prec par [cons@(CDSCons {})]    = render prec par cons
renderSet' prec par cdss                   =
         (text "{ " <> foldl1 (\ a b -> a <> line <>
                                    text ", " <> b)
                                    (map renderFn pairs) <>
                line <> text "}")

   where
        findFn_noUIDs :: CDSSet -> [([CDSSet],CDSSet)]
        findFn_noUIDs c = map (\(a,r,_) -> (a,r)) (findFn c)
        pairs = nub (sort (findFn_noUIDs cdss))
        -- local nub for sorted lists
        nub []                  = []
        nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)              = a : nub as

renderFn :: ([CDSSet],CDSSet) -> Doc
renderFn (args, res)
        = grp  (nest 3
                (text "\\ " <>
                 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b)
                       nil
                       args <> sep <>
                 text "-> " <> renderSet' 0 False res
                )
               )

renderNamedCons :: String -> CDSSet -> Doc
renderNamedCons name cons
  = text name <> nest 2
     ( sep <> linebreak <> grp (text "= " <> renderSet' 0 False cons)
     )

renderNamedFn :: String -> ([CDSSet],CDSSet) -> Doc
renderNamedFn name (args,res)
  = text name <> nest 2
     ( sep <> (foldr (\ a b -> grp (renderSet' 10 False a) <> line <> b) nil args)
       <> linebreak <> grp (text "= " <> renderSet' 0 False res)
     )

findFn :: CDSSet -> [([CDSSet],CDSSet, Maybe UID)]
findFn = foldr findFn' []

findFn' (CDSFun i arg res) rest =
    case findFn res of
       [(args',res',_)] -> (arg : args', res', Just i) : rest
       _                -> ([arg], res, Just i) : rest
findFn' other rest = ([],[other], Nothing) : rest

rmEntry :: CDS -> CDS
rmEntry (CDSNamed str i set) = CDSNamed str i (rmEntrySet set)
rmEntry (CDSCons i str sets) = CDSCons i str (map rmEntrySet sets)
rmEntry (CDSFun i a b)       = CDSFun i (rmEntrySet a) (rmEntrySet b)
rmEntry (CDSTerminated i)    = CDSTerminated i
rmEntry (CDSEntered i)       = error "found bad CDSEntered"

rmEntrySet = map rmEntry . filter noEntered
  where
        noEntered (CDSEntered _) = False
        noEntered _              = True

simplifyCDS :: CDS -> CDS
simplifyCDS (CDSNamed str i set) = CDSNamed str i (simplifyCDSSet set)
simplifyCDS (CDSCons _ "throw"
                  [[CDSCons _ "ErrorCall" set]]
            ) = simplifyCDS (CDSCons 0 "error" set)
simplifyCDS cons@(CDSCons i str sets) =
        case spotString [cons] of
          Just str | not (null str) -> CDSCons 0 (show str) []
          _ -> CDSCons 0 str (map simplifyCDSSet sets)

simplifyCDS (CDSFun i a b) = CDSFun i (simplifyCDSSet a) (simplifyCDSSet b)

simplifyCDS (CDSTerminated i) = (CDSCons i "<?>" [])

simplifyCDSSet = map simplifyCDS

spotString :: CDSSet -> Maybe String
spotString [CDSCons _ ":"
                [[CDSCons _ str []]
                ,rest
                ]
           ]
        = do { ch <- case reads str of
                       [(ch,"")] -> return ch
                       _ -> Nothing
             ; more <- spotString rest
             ; return (ch : more)
             }
spotString [CDSCons _ "[]" []] = return []
spotString other = Nothing

paren :: Bool -> Doc -> Doc
paren False doc = grp ( doc)
paren True  doc = grp ( (text "(" <> doc <> text ")"))

data Output = OutLabel String CDSSet [Output]
            | OutData  CDS
              deriving (Eq,Ord,Show)

cdssToOutput :: CDSSet -> [Output]
cdssToOutput =  map cdsToOutput

cdsToOutput (CDSNamed name _ cdsset)
            = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@(OutLabel {}) <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@(CDSCons {}) = OutData cons
cdsToOutput    fn@(CDSFun {}) = OutData fn

nil = Text.PrettyPrint.FPretty.empty
grp = Text.PrettyPrint.FPretty.group
brk = softbreak -- Nothing, if the following still fits on the current line, otherwise newline. 
sep = softline  -- A space, if the following still fits on the current line, otherwise newline. 
sp :: Doc
sp = text " "   -- A space, always.

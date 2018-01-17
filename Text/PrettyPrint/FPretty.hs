{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe, CPP #-}

-- | 
-- Module      : Text.PrettyPrint.FPretty
-- License     : BSD3
-- Maintainer  : Olaf Chitil <O.Chitil@kent.ac.uk>
-- Portability : portable
--
-- Fast pretty-printing library
--
-- A pretty printer turns a tree structure into indented text, such that the
-- indentation reflects the tree structure. To minimise the number of lines,
-- substructures are put on a single line as far as possible within the given 
-- line-width limit.
--
-- An pretty-printed example with 35 characters line-width:
--
-- > if True
-- >    then if True then True else True
-- >    else
-- >       if False 
-- >          then False 
-- >          else False
--
-- To obtain the above the user of a library only has to convert their tree 
-- structure into a document of type 'Doc'.
--
-- > data Exp = ETrue | EFalse | If Exp Exp Exp
-- >
-- > toDoc :: Exp -> Doc
-- > toDoc ETrue = text "True"
-- > toDoc EFalse = text "False"
-- > toDoc (If e1 e2 e3) =
-- >   group (nest 3 (
-- >     group (nest 3 (text "if" <> line <> toDoc e1)) <> line <>
-- >     group (nest 3 (text "then" <> line <> toDoc e2)) <> line <>
-- >     group (nest 3 (text "else" <> line <> toDoc e3))))
--
-- A document represents a set of layouts. The function 'pretty' then takes
-- a desired maximal printing width and a document and selects the layout that fits
-- best.
--
-- Another example filling lines with elements of a list:
--
-- > list2Doc :: Show a => [a] -> Doc
-- > list2Doc xs = text "[" <> go xs <> text "]"
-- >   where
-- >   go [] = empty
-- >   go [x] = text (show x)
-- >   go (x:y:ys) = text (show x) </> text ", " <> go (y:ys)
-- >
-- > main = putStrLn (pretty 40 (list2Doc [1..20]))
--
-- The output is
--
-- > [1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10
-- > , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18
-- > , 19 , 20]
--
-- FPretty is an implementation of the simple combinators designed by Phil Wadler.
-- The library uses a single associative combinator '<>' to concatenate documents with
-- 'empty' as identity. There is a primitive document for potential line breaks, i.e.,
-- its two layouts are both a line break and a space. The 'group' combinator then
-- enforces that all potential line breaks within a document must be layouted in the
-- same way, i.e. either line breaks or spaces.
--
-- The time complexity is linear in the output size.
-- In contrast, all other pretty printing libraries
-- (original Phil Wadler, PPrint by Leijen, Hughes / Peyton Jones) 
-- use more or less backtracking, and their speed depends unpredictably on the 
-- desired output width.
--
-- Also FPretty provides both relative and absolute indentation via
-- nest and align, whereas HughesPJ provides only relative indentation.
--
-- FPretty uses far less space than other pretty printing libraries for large documents.
-- It does require space linear in the nesting depth of nest/align combinators;
-- however, having these deeply nested leads to a bad layout anyway.
--
-- Unlike other libraries, FPretty does not provide several rendering modes,
-- but could be extended to do so.
--
-- The combinators are a subset of those of PPrint and are similar to HughesPJ
-- to make moving from one library to the other as painless as possible.
-- 
-- For more implementation notes see <http://www.cs.kent.ac.uk/~oc/pretty.html> or
-- Doitse Swierstra and Olaf Chitil: Linear, bounded, functional pretty-printing.
-- Journal of Functional Programming, 19(1):1-16, January 2009.

-- The implementation could be speeded up further by specialisation of the interpreter
-- for dequeues of size 0, 1 and >1.

module Text.PrettyPrint.FPretty 
  (
  -- * The type of documents
   Doc
  -- * Pretty printing
  ,pretty
  -- * Basic documents
  ,empty,text
  -- * Basic documents with several layouts
  ,line,linebreak,softline,softbreak
  -- * Combining two documents
  -- ** The base binary combinator
  ,(<>)
  -- ** Derived binary combinators
  ,(<+>),(<$>),(<$$>),(</>),(<//>)
  -- * Modifying the layouts of one document
  ,group,nest,align,hang
  -- * Combining many documents
  ,hsep,vsep,fillSep,sep,hcat,vcat,fillCat,cat) where

#if __GLASGOW_HASKELL__ >= 710
-- The base libraries from GHC 7.10 onwards export <$> as synonym for fmap.
import Prelude hiding ((<$>))
#endif

import Data.Hashable
import Data.Maybe (fromJust)
import Data.Sequence as Dequeue (Seq, (<|), viewl, viewr, ViewL(..), ViewR(..))
import qualified Data.Sequence as Dequeue (empty)
  -- Originally used Banker's dequeue from Okasaki's book 
  -- on Functional Data Structures.
  -- Efficiency probably similar, but Data.Sequence is part of the standard Haskell 
  -- libraries.
import Data.String
import GHC.Generics

infixr 6 <>,<+>
infixr 5 <$>,<$$>,</>,<//>

----------------------
-- derived combinators

-- | A space, if the following still fits on the current line, otherwise newline.
softline :: Doc
softline = group line

-- | Nothing, if the following still fits on the current line, otherwise newline.
softbreak :: Doc
softbreak = group linebreak

-- | Increase identation relative to the *current* column.
hang :: Int -> Doc -> Doc
hang i x = align (nest i x)

-- | Combine with a space in between.
(<+>) :: Doc -> Doc -> Doc
dl <+> dr = dl <> text " " <> dr

-- | Combine with a 'line' in between.
(<$>) :: Doc -> Doc -> Doc
dl <$> dr = dl <> line <> dr

-- | Combine with a 'linebreak' in between.
(<$$>) :: Doc -> Doc -> Doc
dl <$$> dr = dl <> linebreak <> dr

-- | Combine with a 'softline' in between.
(</>) :: Doc -> Doc -> Doc
dl </> dr = dl <> softline <> dr

-- | Combine with a 'softbreak' in between.
(<//>) :: Doc -> Doc -> Doc
dl <//> dr = dl <> softbreak <> dr

-- ** Combining lists of documents
-- These combinators all assume a non-empty list of documents
-- and they do not start a new line at the end (unlike PPrint).

-- | Combine non-empty list of documents with '<+>', i.e., a space separator.
hsep :: [Doc] -> Doc
hsep = foldr1 (<+>)  -- differs from PPrint

-- | Combine non-empty list of documents with '<$>', i.e., a 'line' separator.
vsep :: [Doc] -> Doc
vsep = foldr1 (<$>)  -- differs from PPrint

-- | Combine non-empty list of documents with '</>', i.e., a 'softline' separator.
fillSep :: [Doc] -> Doc
fillSep = foldr1 (</>)  -- differs from PPrint

-- | Combine non-empty list of documents vertically as a group.
-- Seperated by space instead if all fit on one line.
sep :: [Doc] -> Doc
sep xs = group (vsep xs)  -- differs from PPrint

-- | Combine non-empty list of documents with '<>'.
hcat :: [Doc] -> Doc
hcat = foldr1 (<>)  -- differs from PPrint

-- | Combine non-empty list of documents with '<$$>', i.e., a 'linebreak' separator.
vcat :: [Doc] -> Doc
vcat = foldr1 (<$$>)  -- differs from PPrint

-- | Combine non-empty list of documents with '<//>', i.e., a 'softbreak' separator.
fillCat :: [Doc] -> Doc
fillCat = foldr1 (<//>)   -- differs from PPrint

-- | Combine non-empty list of documents, filling lines as far as possible.
cat :: [Doc] -> Doc
cat xs = group (vcat xs)  -- differs from PPrint

-------------------
-- basic combinators

-- | The empty document; equal to text \"\".
empty :: Doc

-- | Atomic document consisting of just the given text.
-- There should be no newline \\n in the string.
text :: String -> Doc

-- | Either a space or a new line.
line :: Doc

-- | Either nothing ('empty') or a new line.
linebreak :: Doc

-- | Horizontal composition of two documents. 
-- Is associative with identity 'empty'.
(<>) :: Doc -> Doc -> Doc

-- | Mark document as group, that is, layout as a single line if possible.
-- Within a group for all basic documents with several layouts the same layout
-- is chosen, that is, they are all horizontal or all new lines.
-- Within a vertical group there can be a horizontal group, but within a 
-- horizontal group all groups are also layouted horizontally.
group :: Doc -> Doc

-- | Increases current indentation level (absolute). Assumes argument >= 0.
nest :: Int -> Doc -> Doc

-- | Set indentation to current column.
align :: Doc -> Doc

-- | Pretty print within given width.
-- Selects from the *set* of layouts that the document represents the widest
-- that fits within the given width.
-- If no such layout exists, then it will choose the narrowest that exceeds the given
-- width.
pretty :: Int -> Doc -> String

-- | A Document represents a *set* of layouts.
data Doc = Text Int String  -- includes length of text string
         | Nil
         | Line Int String  -- includes length of optional text
         | Doc :<> Doc
         | Group Doc
         | Nest Int Doc     -- increase current indentation
         | Align Int Doc    -- set indentation to current column plus increment
  deriving (Eq, Generic, Ord, Show)

instance Hashable Doc

instance IsString Doc where
  fromString = text

empty = Nil
text t = Text (length t) t
line = Line 1 " "
linebreak = Line 0 ""
(<>) = (:<>)
group = Group
nest = Nest
align = Align 0
pretty w d = interpret (normalise d) w (\p dq r i -> "") 0 Dequeue.empty w [0]


-- semantic-preserving transformation that ensures that between every end
-- of group and a subsequent line there is no text
-- It introduces many superfluous Nils, but it avoids being too strict.
-- Thus e.g. prop3 succeeds.
normalise :: Doc -> Doc
normalise d = td :<> sd
  where
  (td,sd) = go d Nil
  -- Assume second argument only built from text,nil and <>.
  -- Ensures first component of result built only from text,nil and <>.
  -- go d tt = (td,sd) implies  d <> tt and td <> sd denote the same set of 
  -- layouts.
  go :: Doc -> Doc -> (Doc,Doc)
  go Nil tt = (tt,Nil)
  go (Text l t) tt = (Text l t :<> tt,Nil)
  go (Line l t) tt = (Nil,Line l t :<> tt)
  go (dl :<> dr) tt = let (tdl,sdl) = go dl tdr
                          (tdr,sdr) = go dr tt
                      in  (tdl,sdl :<> sdr)
  go (Group d) tt = let (td,sd) = go d tt in (td,Group sd)
  go (Nest i d) tt = let (td,sd) = go d tt in (td,Nest i sd)
  go (Align i d) tt = let (td,sd) = go d tt in (td,Align (i - docLength td) sd)


-- Determine length of a document consisting only of text,nil and <>.
-- To ensure linear complexity for align should actually keep track
-- of document length within go function itself.
docLength :: Doc -> Int
docLength Nil = 0
docLength (Text l _) = l
docLength (dl :<> dr) = docLength dl + docLength dr

type Width = Int
type Position =  Int
type Indentation = Int 
type Horizontal = Bool
type Remaining =  Int
type Out = Remaining -> [Indentation] -> String  
     -- indentation needed here because of align combinator
     -- need list to reset indentation to old value at end of a Nest or Align
type OutGroup = Horizontal -> Out -> Out
type TreeCont = Position -> Dequeue.Seq (Position,OutGroup) -> Out


interpret :: Doc -> Width -> TreeCont -> TreeCont
interpret Nil w tc p ds = tc p ds
interpret (Text l t) w tc p ds =
  extendFrontGroup id prune outText tc (p+l) ds
  where
  outText :: OutGroup
  outText h c r is = t ++ c (r-l) is
interpret (Line l t) w tc p ds =
  extendFrontGroup id prune outLine tc (p+l) ds
  where
  outLine :: OutGroup
  outLine h c r is = if h then t ++ c (r-l) is
                         else '\n' : replicate i ' ' ++ c (w-i) is
    where
    i = head is
interpret (dl :<> dr) w tc p ds =
  interpret dl w (interpret dr w tc) p ds
interpret (Group d) w tc p ds = 
  interpret d w (leaveGroup tc) p ((p,\h c -> c) <| ds)
interpret (Nest j d) w tc p ds =
  extendFrontGroup (interpret d w) (interpret d w) outNest (extendFrontGroup id id outResetIndent tc) p ds
  where
  outNest :: OutGroup
  outNest h c r is = let i = head is in c r (max 0 (i+j) : is)  -- max ensures reasonable behaviour if increment is negative
interpret (Align j d) w tc p ds = 
  extendFrontGroup (interpret d w) (interpret d w) outAlign (extendFrontGroup id id outResetIndent tc) p ds
  where
  outAlign :: OutGroup
  outAlign h c r is = c r (max 0 (w-r+j) : is) -- max ensures reasonable behaviour if increment is negative

outResetIndent :: OutGroup
outResetIndent h c r (i:is) = c r is

-- If no pending groups, then do out directly,
-- otherwise add out to pending group, applying given prune function.
-- This extracts an otherwise repeated pattern of the interpret function.
extendFrontGroup :: (TreeCont -> TreeCont) -> (TreeCont -> TreeCont) -> 
                    OutGroup -> TreeCont -> TreeCont
extendFrontGroup cont1 cont2 out tc p ds =
  case viewl ds of
    EmptyL -> out False (cont1 tc p ds)
    (s,outGrp) :< ds' -> 
      cont2 tc p ((s,\h c -> outGrp h (out h c)) <| ds')


leaveGroup :: TreeCont -> TreeCont
leaveGroup tc p ds = 
  case viewl ds of
    EmptyL -> tc p ds
    (s1,outGrp1) :< ds1 -> 
      case viewl ds1 of
        EmptyL -> outGrp1 True (tc p Dequeue.empty)
        (s2,outGrp2) :< ds2 ->
          tc p ((s2, \f c -> outGrp2 f (\r1 -> outGrp1 (p <= s2+r1) c r1)) <| ds2)


prune :: TreeCont -> TreeCont
prune tc p ds = 
  case viewr ds of
    EmptyR -> tc p ds
    ds' :> (s,outGrp) -> \r -> if p > s+r 
                                 then outGrp False (prune tc p ds') r
                                 else tc p ds r


-- -------------------------------------------
-- Properties for testing. All should be True.

prop = prop0 && prop1 && prop2 && prop3 && prop4 && prop5 && prop6 && 
  prop7 && prop8 && prop9 && prop10 && prop11 && prop12

prop0 = pretty 6 (group (text "Hi" <> line <> text "you") <> text "!") ==
        "Hi\nyou!"
prop1 = pretty 4 (group (text "hi" <> line <> text "world")) ==
        "hi\nworld"
prop2 = 
  pretty 8 (group (text "hi" <> line <> text "world") <> text "liness") ==
  "hi\nworldliness"
prop3 = 
  take 6 (pretty 4 (group (text "hi" <> line <> text "you" <> undefined))) ==
  "hi\nyou"
prop4 = 
  take 6 (pretty 4 (group (text "hi" <> line) <>
           group (text "you" <> line) <> undefined)) ==
  "hi\nyou"
prop5 = 
  take 6 (pretty 4 (group (text "hi" <> 
           group (line <> text "you" <> undefined)))) ==
  "hi\nyou"
prop6 = 
  take 7 (pretty 3 (group (text "hi" <> line <> 
           group (line <> text "you" <> undefined)))) ==
  "hi\n\nyou"
prop7 = 
  pretty 10 (group (text "what" <>
    align (group (text "do" <> line <> text "you" <> line <> 
      text "do" <> align (line <> text "now?"))))) ==
  "whatdo\n    you\n    do\n      now?"

prop8 = 
  pretty 10 (group (text "one " <> (align (line <> text "two" <> 
    align (line <> text "three"))))) ==
  "one \n    two\n       three"

prop9 =
  pretty 10 (group (text "one " <> (nest 2 (line <> text "two" <>
    nest 3 (line <> text "three"))))) ==
  "one \n  two\n     three"

prop10 =
  pretty 5 (group (nest 2 (text "one" <> line <> text "two")) <>
    group (line <> text "three")) ==
  "one\n  two\nthree"

prop11 =
  pretty 10 (text "one" <> softline <> (align (group (text "two" <> line <> text "three"))) <>
    softline <> text "four") ==
  "one two\n    three\nfour"

prop12 =
  pretty 15 (group ( text "this"
                 <> nest 9 (line <> group (text "takes" <> line <> text "four")) 
                 <> line <> text "lines")) ==
  "this\n         takes\n         four\nlines"

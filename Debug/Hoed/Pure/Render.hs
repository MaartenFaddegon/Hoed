-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014
{-# LANGUAGE CPP #-}

module Debug.Hoed.Pure.Render
(CompStmt(..)
,renderCompStmts
,CDS
,eventsToCDS
,rmEntrySet
,simplifyCDSSet
) where
import Debug.Hoed.Pure.EventForest

import Prelude hiding(lookup)
import Debug.Hoed.Pure.Observe
import Data.List(sort,sortBy,partition,nub
#if __GLASGOW_HASKELL__ >= 710
                , sortOn
#endif
                )
import Data.Graph.Libgraph
import Data.Array as Array

head' :: String -> [a] -> a
head' msg [] = error msg
head' _   xs = head xs

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
                deriving (Eq, Ord)

instance Show CompStmt where
  show = stmtRes
  showList eqs eq = unlines (map show eqs) ++ eq


------------------------------------------------------------------------
-- Render equations from CDS set

renderCompStmts :: CDSSet -> [CompStmt]
renderCompStmts = foldl (\acc set -> acc ++ renderCompStmt set) []

-- renderCompStmt: an observed function can be applied multiple times, each application
-- is rendered to a computation statement

renderCompStmt :: CDS -> [CompStmt]
renderCompStmt (CDSNamed name threadId dependsOn set uids')
  = map mkStmt statements
  where statements :: [(String,UID)]
        statements   = map (\(d,i) -> (pretty 120 d,i)) doc
        doc          = foldl (\a b -> a ++ renderNamedTop name b) [] output
        output       = cdssToOutput set

        mkStmt :: (String,UID) -> CompStmt
        mkStmt (s,i) = CompStmt name i s

renderNamedTop :: String -> Output -> [(DOC,UID)]
renderNamedTop name (OutData cds)
  =  map (\(args,res,Just i) -> (renderNamedFn name (args,res), i)) pairs

  where pairs' = findFn [cds]
        pairs  = (nub . sortOn argAndRes) pairs'
        -- local nub for sorted lists
        nub []                  = []
        nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)              = a : nub as

        argAndRes (arg,res,_) = (arg,res)

-- %************************************************************************
-- %*                                                                   *
-- \subsection{The CDS and converting functions}
-- %*                                                                   *
-- %************************************************************************


data CDS = CDSNamed      String ThreadId UID CDSSet [UID]
         | CDSCons       UID    String   [CDSSet]
         | CDSFun        UID             CDSSet CDSSet
         | CDSEntered    UID
         | CDSTerminated UID
        deriving (Show,Eq,Ord)

type CDSSet = [CDS]

eventsToCDS :: [Event] -> CDSSet
eventsToCDS pairs = getChild 0 0
   where
     frt :: EventForest
     frt = mkEventForest pairs

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
        (Observe str t i) -> let chd = getChild node 0
                               in CDSNamed str t (getId chd i) chd (treeUIDs frt e)
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

render  :: Int -> Bool -> CDS -> DOC
render prec par (CDSCons _ ":" [cds1,cds2]) =
        if (par && not needParen)  
        then doc -- dont use paren (..) because we dont want a grp here!
        else paren needParen doc
   where
        doc = grp (brk <> renderSet' 5 False cds1 <> text " : ") <>
              renderSet' 4 True cds2
        needParen = prec > 4
render prec par (CDSCons _ "," cdss) | length cdss > 0 =
        nest 2 (text "(" <> foldl1 (\ a b -> a <> text ", " <> b)
                            (map renderSet cdss) <>
                text ")")
render prec par (CDSCons _ name cdss) =
        paren (length cdss > 0 && prec /= 0)
              (nest 2
                 (text name <> foldr (<>) nil
                                [ sep <> renderSet' 10 False cds
                                | cds <- cdss 
                                ]
                 )
              )

{- renderSet handles the various styles of CDSSet.
 -}

renderSet :: CDSSet -> DOC
renderSet = renderSet' 0 False

renderSet' :: Int -> Bool -> CDSSet -> DOC
renderSet' _ _      [] = text "_"
renderSet' prec par [cons@(CDSCons {})]    = render prec par cons
renderSet' prec par cdss                   = 
        nest 0 (text "{ " <> foldl1 (\ a b -> a <> line <>
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

renderFn :: ([CDSSet],CDSSet) -> DOC
renderFn (args, res)
        = grp  (nest 3 
                (text "\\ " <>
                 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b)
                       nil
                       args <> sep <>
                 text "-> " <> renderSet' 0 False res
                )
               )

renderNamedFn :: String -> ([CDSSet],CDSSet) -> DOC
renderNamedFn name (args,res)
  = grp (nest 3 
            (  text name <> sep
            <> foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b) nil args 
            <> sep <> text "= " <> renderSet' 0 False res
            )
        )

findFn :: CDSSet -> [([CDSSet],CDSSet, Maybe UID)]
findFn = foldr findFn' []

findFn' (CDSFun i arg res) rest =
    case findFn res of
       [(args',res',_)] -> (arg : args', res', Just i) : rest
       _                -> ([arg], res, Just i) : rest
findFn' other rest = ([],[other], Nothing) : rest

renderTops []   = nil
renderTops tops = line <> foldr (<>) nil (map renderTop tops)

renderTop :: Output -> DOC
renderTop (OutLabel str set extras) =
        nest 2 (text ("-- " ++ str) <> line <>
                renderSet set
                <> renderTops extras) <> line

rmEntry :: CDS -> CDS
rmEntry (CDSNamed str t i set us)= CDSNamed str t i (rmEntrySet set) us
rmEntry (CDSCons i str sets)       = CDSCons i str (map rmEntrySet sets)
rmEntry (CDSFun i a b)             = CDSFun i (rmEntrySet a) (rmEntrySet b)
rmEntry (CDSTerminated i)          = CDSTerminated i
rmEntry (CDSEntered i)             = error "found bad CDSEntered"

rmEntrySet = map rmEntry . filter noEntered
  where
        noEntered (CDSEntered _) = False
        noEntered _              = True

simplifyCDS :: CDS -> CDS
simplifyCDS (CDSNamed str t i set us) = CDSNamed str t i (simplifyCDSSet set) us
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

paren :: Bool -> DOC -> DOC
paren False doc = grp (nest 0 doc)
paren True  doc = grp (nest 0 (text "(" <> nest 0 doc <> brk <> text ")"))

sp :: DOC
sp = text " "

data Output = OutLabel String CDSSet [Output]
            | OutData  CDS
              deriving (Eq,Ord,Show)


commonOutput :: [Output] -> [Output]
commonOutput = sortBy byLabel
  where
     byLabel (OutLabel lab _ _) (OutLabel lab' _ _) = compare lab lab'

cdssToOutput :: CDSSet -> [Output]
cdssToOutput =  map cdsToOutput

cdsToOutput (CDSNamed name _ _ cdsset _)
            = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@(OutLabel {}) <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@(CDSCons {}) = OutData cons
cdsToOutput    fn@(CDSFun {}) = OutData fn

-- %************************************************************************
-- %*                                                                   *
-- \subsection{A Pretty Printer}
-- %*                                                                   *
-- %************************************************************************

-- This pretty printer is based on Wadler's pretty printer.

data DOC                = NIL                   -- nil    
                        | DOC :<> DOC           -- beside 
                        | NEST Int DOC
                        | TEXT String
                        | LINE                  -- always "\n"
                        | SEP                   -- " " or "\n"
                        | BREAK                 -- ""  or "\n"
                        | DOC :<|> DOC          -- choose one
                        deriving (Eq,Show)
data Doc                = Nil
                        | Text Int String Doc
                        | Line Int Int Doc
                        deriving (Show,Eq)


mkText                  :: String -> Doc -> Doc
mkText s d              = Text (toplen d + length s) s d

mkLine                  :: Int -> Doc -> Doc
mkLine i d              = Line (toplen d + i) i d

toplen                  :: Doc -> Int
toplen Nil              = 0
toplen (Text w s x)     = w
toplen (Line w s x)     = 0

nil                     = NIL
x <> y                  = x :<> y
nest i x                = NEST i x
text s                  = TEXT s
line                    = LINE
sep                     = SEP
brk                     = BREAK

fold x                  = grp (brk <> x)

grp                     :: DOC -> DOC
grp x                   = 
        case flatten x of
          Just x' -> x' :<|> x
          Nothing -> x

flatten                 :: DOC -> Maybe DOC
flatten NIL             = return NIL
flatten (x :<> y)       = 
        do x' <- flatten x
           y' <- flatten y
           return (x' :<> y')
flatten (NEST i x)      = 
        do x' <- flatten x
           return (NEST i x')
flatten (TEXT s)        = return (TEXT s)
flatten LINE            = Nothing               -- abort
flatten SEP             = return (TEXT " ")     -- SEP is space
flatten BREAK           = return NIL            -- BREAK is nil
flatten (x :<|> y)      = flatten x

layout                  :: Doc -> String
layout Nil              = ""
layout (Text _ s x)     = s ++ layout x
layout (Line _ i x)     = '\n' : replicate i ' ' ++ layout x

best w k doc = be w k [(0,doc)]

be                      :: Int -> Int -> [(Int,DOC)] -> Doc
be w k []               = Nil
be w k ((i,NIL):z)      = be w k z
be w k ((i,x :<> y):z)  = be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z) = be w k ((k+j,x):z)
be w k ((i,TEXT s):z)   = s `mkText` be w (k+length s) z
be w k ((i,LINE):z)     = i `mkLine` be w i z
be w k ((i,SEP):z)      = i `mkLine` be w i z
be w k ((i,BREAK):z)    = i `mkLine` be w i z
be w k ((i,x :<|> y):z) = better w k 
                                (be w k ((i,x):z))
                                (be w k ((i,y):z))

better                  :: Int -> Int -> Doc -> Doc -> Doc
better w k x y          = if (w-k) >= toplen x then x else y

pretty                  :: Int -> DOC -> String
pretty w x              = layout (best w 0 x)

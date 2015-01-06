-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014

module Debug.Hoed.Render
(CompStmt(..)
,renderCompStmts
,byStack
,showWithStack

,CompGraph(..)
,Vertex(..)
,mkGraph

,CDS
,eventsToCDS
,rmEntrySet
,simplifyCDSSet
,isRoot
)
where

import Prelude hiding(lookup)
import Debug.Hoed.Observe
import Data.List(sort,sortBy,partition)
import Data.Graph.Libgraph
import Data.Array as Array
import Data.Tree.RBTree(RBTree(..),insertOrd,emptyRB,search)

------------------------------------------------------------------------
-- Render equations from CDS set

renderCompStmts :: CDSSet -> [CompStmt]
renderCompStmts = map renderCompStmt

renderCompStmt :: CDS -> CompStmt
renderCompStmt (CDSNamed name set)
  = CompStmt name equation (head stack)
  where equation    = pretty 120 (commas doc)
        (doc,stack) = unzip rendered
        rendered    = map (renderNamedTop name) output
        output      = cdssToOutput set
        commas [d]  = d
        commas ds   = (foldl (\acc d -> acc <> d <> text ", ") (text "{") ds) <> text "}"

renderCompStmt _ = CompStmt "??" "??" emptyStack

renderNamedTop :: String -> Output -> (DOC,CallStack)
renderNamedTop name (OutData cds)
  = ( nest 2 $ foldl1 (\ a b -> a <> line <> text ", " <> b) (map (renderNamedFn name) pairs)
    , callStack
    )
  where (pairs',callStack) = findFn [cds] 
        pairs           = (nub . (sort)) pairs'
	-- local nub for sorted lists
	nub []                  = []
	nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)              = a : nub as

renderCallStack :: CallStack -> DOC
renderCallStack s
  =  text "With call stack: ["
  <> foldl1 (\a b -> a <> text ", " <> b) 
            (map text s)
  <> text "]"

------------------------------------------------------------------------
-- The CompStmt type

data CompStmt = CompStmt {equLabel :: String, equRes :: String, equStack :: CallStack}
                deriving (Eq, Ord)

instance Show CompStmt where
  show e = equRes e -- ++ " with stack " ++ show (equStack e)
  showList eqs eq = unlines (map show eqs) ++ eq

showWithStack :: [CompStmt] -> String
showWithStack eqs = unlines (map show' eqs)
  where show' eq
         = equRes eq ++ "\n\tWith call stack: " ++ showStack (equStack eq)
                     ++ "\n\tNext stack:      " ++ showStack (nextStack eq)
           where showStack [] = "[-]"
                 showStack ss = (foldl (\s' s -> s' ++ s ++ ",") "[" ss) ++ "-]"

-- Compare equations by stack
byStack e1 e2
    = case compareStack (equStack e1) (equStack e2) of
        EQ -> compare (equLabel e1) (equLabel e2)
        d  -> d

compareStack s1 s2
  | l1 < l2  = LT
  | l1 > l2  = GT
  | l1 == l2 = c (zip s1 s2)
  where l1 = length s1
        l2 = length s2
        c []         = EQ
        c ((x,y):ss) = case compare x y of
          EQ -> c ss
          d  -> d
              
------------------------------------------------------------------------
-- Stack matching

-- nextStack = case getPushMode of
--         Vanilla  -> nextStack_vanilla
--         Drop     -> nextStack_drop
--         Truncate -> nextStack_truncate
nextStack = nextStack_truncate

-- Always push onto top of stack
nextStack_vanilla :: CompStmt -> CallStack
nextStack_vanilla (CompStmt cc _ ccs) = cc:ccs

-- Drop on recursion
nextStack_drop :: CompStmt -> CallStack
nextStack_drop (CompStmt cc _ [])   = [cc]
nextStack_drop (CompStmt cc _ ccs)
  = if ccs `contains` cc 
        then ccs
        else cc:ccs

-- Remove everything between recursion (e.g. [f,g,f,h] becomes [f,h])
nextStack_truncate :: CompStmt -> CallStack
nextStack_truncate (CompStmt cc _ [])   = [cc]
nextStack_truncate (CompStmt cc _ ccs)
  = if ccs `contains` cc 
        then dropWhile (/= cc) ccs
        else cc:ccs

contains :: CallStack -> String -> Bool
contains ccs cc = filter (== cc) ccs /= []

call :: CallStack -> CallStack -> CallStack
call sApp sLam = sLam' ++ sApp
  where (sPre,sApp',sLam') = commonPrefix sApp sLam

commonPrefix :: CallStack -> CallStack -> (CallStack, CallStack, CallStack)
commonPrefix sApp sLam
  = let (sPre,sApp',sLam') = span2 (==) (reverse sApp) (reverse sLam)
    in (sPre, reverse sApp', reverse sLam') 

span2 :: (a -> a -> Bool) -> [a] -> [a] -> ([a], [a], [a])
span2 f = s f []
  where s _ pre [] ys = (pre,[],ys)
        s _ pre xs [] = (pre,xs,[])
        s f pre xs@(x:xs') ys@(y:ys') 
          | f x y     = s f (x:pre) xs' ys'
          | otherwise = (pre,xs,ys)

------------------------------------------------------------------------
-- Bags are collections of computation statements with the same stack

data Bag = Bag {bagStack :: CallStack, bagStmts :: [CompStmt]}

instance Eq  Bag where b1 == b2 = bagStack b1 == bagStack b2
instance Ord Bag where compare b1 b2 = cmpStk (bagStack b1) (bagStack b2)

bag :: (CompStmt -> CallStack) -> CompStmt -> Bag
bag s c = Bag (s c) [c]

(+++) :: CompStmt -> Bag -> Bag
c +++ b = b{bagStmts = c : bagStmts b}


mkBags :: (CompStmt -> CallStack) -> [CompStmt] -> [Bag]
mkBags _ []  = []
mkBags s cs = mkBags' (bag s fc) [] ocs

  where (fc:ocs) = sortBy (\c1 c2 -> cmpStk (s c1) (s c2)) cs

        mkBags' b bs []     = b:bs
        mkBags' b bs (c:cs)
          | bagStack b == s c = mkBags' (c +++ b) bs     cs
          | otherwise        = mkBags' (bag s c) (b:bs) cs

------------------------------------------------------------------------
-- RB trees to speed up computation graph construction

type Tree = RBTree Bag

mkTree :: (CompStmt -> CallStack) -> [CompStmt] -> Tree
mkTree s cs = foldl insertOrd emptyRB (mkBags s cs)

mkTrees :: [CompStmt] -> (Tree,Tree)
mkTrees cs = (mkTree equStack cs, mkTree nextStack cs)


cmpStk :: CallStack -> CallStack -> Ordering
cmpStk [] [] = EQ
cmpStk s1 s2 = case compare (length s1) (length s2) of
                EQ  -> compare (head s1) (head s2)
                ord -> ord

lookup :: Tree -> CallStack -> [CompStmt]
lookup t s = case search (\s b -> cmpStk s (bagStack b)) t s of
               (Just b) -> bagStmts b
               Nothing  -> []

------------------------------------------------------------------------
-- Inverse call

stmts :: Tree -> (CallStack,CallStack) -> [(CompStmt,CompStmt)]
stmts t (s1,s2) = flattenStmts (lookup t s1, lookup t s2)

flattenStmts :: ([a],[b]) -> [(a,b)]
flattenStmts (xs,ys) = foldl (\zs x->foldl (\zs' y->(x,y) : zs') zs ys) [] xs

lacc :: CallStack -> [(CallStack,CallStack)]
lacc = expand . split

expand :: [([a],[a])] -> [([a],[a])]
expand xs = foldl (\ys x -> ys ++ map (cat $ snd x) (expand' x)) [] xs

  where expand' (xs,[])   = [(xs,[])]
        expand' (xs,y:ys) = (xs,y:ys) : expand' (xs,ys)

        cat sApp (xs,ys) = (sApp,xs++ys)

split :: [a] -> [([a],[a])]
split []     = []
split (x:xs) = split' [x] xs []

  where split' _  []     zs = zs
        split' xs (y:ys) zs = split' (xs++[y]) ys ((xs,y:ys):zs)

------------------------------------------------------------------------
-- Computation graphs

data Vertex = Root | Vertex {equations :: [CompStmt], status :: Judgement}
              deriving (Eq,Show,Ord)

type CompGraph = Graph Vertex ()

isRoot Root = True
isRoot _    = False

pushDeps :: (Tree,Tree) -> [CompStmt] -> [Arc CompStmt ()]
pushDeps ts cs = concat (map (pushArcs ts) cs)

pushArcs :: (Tree,Tree) -> CompStmt -> [Arc CompStmt ()]
pushArcs t p = map (\c -> p ==> c) (pushers t p)
  where src ==> tgt = Arc src tgt ()

pushers :: (Tree,Tree) -> CompStmt -> [CompStmt]
pushers (ts,tn) c = lookup ts (nextStack c)

callDeps :: (Tree,Tree) -> [CompStmt] -> [Arc CompStmt ()]
callDeps (_,t) cs = concat (map (callDep t) cs)

callDep :: Tree -> CompStmt -> [Arc CompStmt ()]
callDep t c3 = foldl (\as (c2,c1) -> c1 ==> c2 : c2 ==> c3 : as) []
                          (concat (map (stmts t) (lacc $ equStack c3)))
  where src ==> tgt = Arc src tgt ()

mkGraph :: [CompStmt] -> CompGraph
mkGraph cs = {-# SCC "mkGraph" #-} (dagify merge) . addRoot . toVertices $ g
  where g :: Graph CompStmt ()
        g = let ts = mkTrees cs in Graph (head cs) cs (pushDeps ts cs ++ callDeps ts cs)

        toVertices :: Graph CompStmt () -> CompGraph
        toVertices = mapGraph (\s->Vertex [s] Unassessed)

        addRoot :: CompGraph -> CompGraph
        addRoot (Graph _ vs as) =
                let rs = filter (\(Vertex (s:_) _) -> equStack s == []) vs
                    es = map (\r -> Root ==> r) rs
                in  Graph Root (Root : vs) (es ++ as)

        merge :: [Vertex] -> Vertex
        merge vs = let v = head vs; cs' = map equations vs
                   in v{equations=foldl (++) [] cs'}

        src ==> tgt = Arc src tgt ()

-- %************************************************************************
-- %*									*
-- \subsection{The CDS and converting functions}
-- %*									*
-- %************************************************************************


data CDS = CDSNamed String         CDSSet
	 | CDSCons Int String     [CDSSet]
	 | CDSFun  Int             CDSSet CDSSet CallStack
	 | CDSEntered Int
	 | CDSTerminated Int
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

     out_arr = array bnds	-- never uses 0 index
	        [ (node,getNode'' node change)
	 	| (Event node _ change) <- pairs
		]

     getNode'' ::  Int -> Change -> CDS
     getNode'' node change =
       case change of
	(Observe str) -> CDSNamed str (getChild node 0)
	(Enter)       -> CDSEntered node
	(NoEnter)     -> CDSTerminated node
	(Fun str)     -> CDSFun node (getChild node 0) (getChild node 1) str
	(Cons portc cons)
		      -> CDSCons node cons 
				[ getChild node n | n <- [0..(portc-1)]]

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
renderSet' prec par cdss		   = 
	nest 0 (text "{ " <> foldl1 (\ a b -> a <> line <>
				    text ", " <> b)
				    (map (renderFn caller) pairs) <>
	        line <> text "}")

   where
	(pairs',caller) = findFn cdss
        pairs           = (nub . sort) pairs'
	-- local nub for sorted lists
	nub []                  = []
	nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)              = a : nub as

renderFn :: CallStack -> ([CDSSet],CDSSet) -> DOC
renderFn callStack (args, res)
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


-- This is where the call stacks are merged.
--
-- MF TODO: It would be beneficial for performance if we would only save the
-- stack once at the top as we already do in the paper and our semantics test code

findFn :: CDSSet -> ([([CDSSet],CDSSet)], CallStack)
findFn = foldr findFn' ([],[])

findFn' (CDSFun _ arg res caller) (rest,_) =
    case findFn res of
       ([(args',res')],caller') -> if caller' /= [] && caller' /= caller 
                                   then error "found two different stacks!"
                                   else ((arg : args', res') : rest, caller)
       _                        -> (([arg], res) : rest,        caller)
findFn' other (rest,caller)   =  (([],[other]) : rest,        caller)

renderTops []   = nil
renderTops tops = line <> foldr (<>) nil (map renderTop tops)

renderTop :: Output -> DOC
renderTop (OutLabel str set extras) =
	nest 2 (text ("-- " ++ str) <> line <>
		renderSet set
		<> renderTops extras) <> line

rmEntry :: CDS -> CDS
rmEntry (CDSNamed str set)   = CDSNamed str (rmEntrySet set)
rmEntry (CDSCons i str sets) = CDSCons i str (map rmEntrySet sets)
rmEntry (CDSFun i a b str)   = CDSFun i (rmEntrySet a) (rmEntrySet b) str
rmEntry (CDSTerminated i)    = CDSTerminated i
rmEntry (CDSEntered i)       = error "found bad CDSEntered"

rmEntrySet = map rmEntry . filter noEntered
  where
	noEntered (CDSEntered _) = False
	noEntered _              = True

simplifyCDS :: CDS -> CDS
simplifyCDS (CDSNamed str set) = CDSNamed str (simplifyCDSSet set)
simplifyCDS (CDSCons _ "throw" 
		  [[CDSCons _ "ErrorCall" set]]
	    ) = simplifyCDS (CDSCons 0 "error" set)
simplifyCDS cons@(CDSCons i str sets) = 
	case spotString [cons] of
	  Just str | not (null str) -> CDSCons 0 (show str) []
	  _ -> CDSCons 0 str (map simplifyCDSSet sets)

simplifyCDS (CDSFun i a b str) = CDSFun 0 (simplifyCDSSet a) (simplifyCDSSet b) str

simplifyCDS (CDSTerminated i) = (CDSCons 0 "<?>" [])

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

cdsToOutput (CDSNamed name cdsset)
	    = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@(OutLabel {}) <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@(CDSCons {}) = OutData cons
cdsToOutput    fn@(CDSFun {}) = OutData fn

-- %************************************************************************
-- %*									*
-- \subsection{A Pretty Printer}
-- %*									*
-- %************************************************************************

-- This pretty printer is based on Wadler's pretty printer.

data DOC		= NIL			-- nil	  
			| DOC :<> DOC		-- beside 
			| NEST Int DOC
			| TEXT String
			| LINE			-- always "\n"
			| SEP			-- " " or "\n"
			| BREAK			-- ""  or "\n"
			| DOC :<|> DOC		-- choose one
			deriving (Eq,Show)
data Doc		= Nil
			| Text Int String Doc
			| Line Int Int Doc
			deriving (Show,Eq)


mkText			:: String -> Doc -> Doc
mkText s d		= Text (toplen d + length s) s d

mkLine			:: Int -> Doc -> Doc
mkLine i d		= Line (toplen d + i) i d

toplen			:: Doc -> Int
toplen Nil		= 0
toplen (Text w s x)	= w
toplen (Line w s x)	= 0

nil			= NIL
x <> y			= x :<> y
nest i x		= NEST i x
text s 			= TEXT s
line			= LINE
sep			= SEP
brk			= BREAK

fold x			= grp (brk <> x)

grp 			:: DOC -> DOC
grp x			= 
	case flatten x of
	  Just x' -> x' :<|> x
	  Nothing -> x

flatten 		:: DOC -> Maybe DOC
flatten	NIL		= return NIL
flatten (x :<> y)	= 
	do x' <- flatten x
	   y' <- flatten y
	   return (x' :<> y')
flatten (NEST i x)	= 
	do x' <- flatten x
	   return (NEST i x')
flatten (TEXT s)	= return (TEXT s)
flatten LINE		= Nothing		-- abort
flatten SEP		= return (TEXT " ")	-- SEP is space
flatten BREAK		= return NIL		-- BREAK is nil
flatten (x :<|> y)	= flatten x

layout 			:: Doc -> String
layout Nil		= ""
layout (Text _ s x)	= s ++ layout x
layout (Line _ i x)	= '\n' : replicate i ' ' ++ layout x

best w k doc = be w k [(0,doc)]

be 			:: Int -> Int -> [(Int,DOC)] -> Doc
be w k []		= Nil
be w k ((i,NIL):z)	= be w k z
be w k ((i,x :<> y):z)	= be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z) = be w k ((k+j,x):z)
be w k ((i,TEXT s):z)	= s `mkText` be w (k+length s) z
be w k ((i,LINE):z)	= i `mkLine` be w i z
be w k ((i,SEP):z)	= i `mkLine` be w i z
be w k ((i,BREAK):z)	= i `mkLine` be w i z
be w k ((i,x :<|> y):z) = better w k 
				(be w k ((i,x):z))
				(be w k ((i,y):z))

better			:: Int -> Int -> Doc -> Doc -> Doc
better w k x y		= if (w-k) >= toplen x then x else y

pretty			:: Int -> DOC -> String
pretty w x		= layout (best w 0 x)

-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014

module Debug.Hoed.Render
where

import Debug.Hoed.Observe
import Data.List(sort,sortBy,partition)
import Data.Graph.Libgraph
import Data.Array as Array

------------------------------------------------------------------------
-- Render equations from CDS set

renderCompStmts :: CDSSet -> [CompStmt]
renderCompStmts = map renderCompStmt

renderCompStmt :: CDS -> CompStmt
renderCompStmt (CDSNamed name set)
  = CompStmt name equation (head stack)                    -- MF TODO: head?
  where equation    =  pretty 40 (foldr (<>) nil doc)
        (doc,stack) = unzip rendered
        rendered    = map (renderNamedTop name) output
        output      = cdssToOutput set
        -- MF TODO: Do we want to sort?
        -- output      = (commonOutput . cdssToOutput) set
renderCompStmt _ = CompStmt "??" "??" emptyStack

renderNamedTop :: String -> Output -> (DOC,CallStack)
renderNamedTop name (OutData cds)
  = ( nest 2 (  foldl1 (\ a b -> a <> line <> text ", " <> b)
                (map (renderNamedFn name) pairs)
           -- <> sep <> nest 2 (renderCallStack callStack)
           )
      -- <> line
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
-- Computation graphs

data Vertex = Vertex {equations :: [CompStmt], status :: Judgement}
              deriving (Eq,Show,Ord)

data Dependency = PushDep | CallDep Int deriving (Eq,Show,Ord)

type CompGraph = Graph Vertex Dependency

mkGraph :: [CompStmt] -> CompGraph
mkGraph trc 
  = {-# SCC "mkGraph" #-}
    dagify merge $ mapGraph (\r->Vertex [r] Unassessed) (mkGraph' trc)
  where merge :: [Vertex] -> Vertex
        merge vs = let v = head vs; stmts = map equations vs
                   in v{equations=foldl (++) [] stmts}


mkGraph' stmts 
  = {-# SCC "mkGraph'" #-} Graph (head roots) stmts arcs

  where roots = {-# SCC "roots" #-} filter (\s -> equStack s == []) stmts

        -- arcsFrom is called O(N) times with N number of statements
        arcs = (nubMin $ foldr (\r as -> (arcsFrom r stmts) ++ as) [] stmts)


nubMin :: (Eq a, Ord b) => [Arc a b] -> [Arc a b]
nubMin l = nub' l []
  where

    nub' [] _           = []
    nub' (x:xs) ls = let (sat,unsat) = partition (equiv x) ls
                     in case sat of
                        [] -> x : nub' xs (x:ls)
                        _  -> nub' xs ((minimum' $ x:sat) : unsat )

    minimum' as = (head as) { arc = minimum (map arc as) }

    equiv (Arc v w _) (Arc x y _) = v == x && w == y

-- arcsFrom has a complexity of O(N*N) with N number of statements
arcsFrom :: CompStmt -> [CompStmt] -> [Arc CompStmt Dependency]
arcsFrom src stmts
  = {-# SCC "arcsFrom" #-}
     ((map (\tgt -> Arc src tgt PushDep))     . (filter isPushArc)  $ stmts)
  ++ ((map (\tgt -> Arc src tgt (CallDep 1))) . (filter isCall1Arc) $ stmts)

  where -- isPushArc is O(1)
        isPushArc tgt = nextStack src == equStack tgt

        -- isCall1Arc is O(N) for number of stmts
        isCall1Arc tgt = anyOf (map (callDependency src) stmts) tgt

-- anyOf is O(P) for P number of predicates
anyOf :: [a->Bool] -> a -> Bool
anyOf ps x = or (map (\p -> p x) ps)

-- callDependency is O(1)
callDependency :: CompStmt -> CompStmt -> CompStmt -> Bool
callDependency pLam pApp c 
  = call (nextStack pApp) (nextStack pLam) == equStack c

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
                 -- <> text (" <<" ++ renderCallStack callStack ++ ">>")
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


-- MF TODO: Not sure if this is ok, we only remember one call stack
-- are they truly all the same?

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
	-- replace with 
	-- 	CDSCons i "->" [simplifyCDSSet a,simplifyCDSSet b]
	-- for turning off the function stuff.

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

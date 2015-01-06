{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.List(sort,sortBy,partition)
import Data.Array as Array
import Debug.Hoed(gdmobserve,Observable(..),runO,Generic)

------------------------------------------------------------------------
-- Make our datatypes observable

instance Observable CDS
instance Observable DOC
instance Observable CompStmt
instance Observable Output

------------------------------------------------------------------------
-- The main program with the failing testcase.

main = runO [] $ print (renderCompStmts cdss)

cdss = [CDSNamed "f" [CDSFun 0 [CDSCons 0 "2" []] [CDSCons 0 "1" []] [],CDSFun 0 [CDSCons 0 "0" []] [CDSCons 0 "0" []] []],CDSNamed "g" [CDSFun 0 [CDSCons 0 "2" []] [CDSCons 0 "1" []] ["f"]]]

------------------------------------------------------------------------
-- Render equations from CDS set

renderCompStmts :: CDSSet -> [CompStmt]
renderCompStmts = gdmobserve "renderCompStmts" ({-# SCC "renderCompStmts" #-} map renderCompStmt)

renderCompStmt :: CDS -> CompStmt
renderCompStmt c' = gdmobserve "renderCompStmt" (\c-> {-# SCC "renderCompStmt" #-} renderCompStmt' c) c'
renderCompStmt' (CDSNamed name set) = CompStmt name equation (head stack)
  where equation    = pretty 80 (foldr (<>) nil doc) -- BUG: foldr (<>) just puts equations
                                                     -- beside eachother, rather than seperating
                                                     -- them with commas
        (doc,stack) = unzip rendered
        rendered    = map (renderNamedTop name) output
        output      = cdssToOutput set
        -- MF TODO: Do we want to sort?
        -- output      = (commonOutput . cdssToOutput) set
renderCompStmt' _ = CompStmt "??" "??" emptyStack

renderNamedTop :: String -> Output -> (DOC,CallStack)
renderNamedTop arg1 arg2 = gdmobserve "renderNamedTop" 
                           (\arg1' arg2' -> {-# SCC "renderNamedTop" #-} renderNamedTop' arg1' arg2'
                           ) arg1 arg2
renderNamedTop' name (OutData cds)
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
                deriving (Eq, Ord, Generic)

instance Show CompStmt where
  show e = equRes e -- ++ " with stack " ++ show (equStack e)
  showList eqs eq = unlines (map show eqs) ++ eq

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

-- The CDS and converting functions

data CDS = CDSNamed String         CDSSet
	 | CDSCons Int String     [CDSSet]
	 | CDSFun  Int             CDSSet CDSSet CallStack
	 | CDSEntered Int
	 | CDSTerminated Int
	deriving (Show,Eq,Ord,Generic)

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
renderNamedFn arg1 arg2 = gdmobserve "renderNamedFn" 
                                (\arg1' arg2' -> {-# SCC "renderNamedFn" #-}
                                renderNamedFn' arg1' arg2') arg1 arg2
renderNamedFn' name (args,res)
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
	      deriving (Eq,Ord,Show,Generic)


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
			deriving (Eq,Show,Generic)
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
pretty w x = gdmobserve "pretty" (\w' x'-> {-# SCC "pretty" #-} pretty' w' x') w x
pretty' w x		= layout (best w 0 x)

------------------------------------------------------------------------
-- Stacks

emptyStack = [""]
type CallStack = [String]

------------------------------------------------------------------------
-- Events
data Event = Event
		{ portId     :: !Int
		, parent     :: !Parent
		, change     :: !Change
		}
	deriving (Show, Read)

data Change
	= Observe 	!String
	| Cons    !Int 	!String
	| Enter
        | NoEnter
	| Fun           !CallStack
	deriving (Show, Read)

data Parent = Parent
	{ observeParent :: !Int	-- my parent
	, observePort   :: !Int	-- my branch number
	} deriving (Show, Read)
root = Parent 0 0

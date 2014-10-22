\begin{code}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

\end{code}

The file is part of the Haskell Object Observation Debugger,
(HOOD) March 2010 release.

HOOD is a small post-mortem debugger for the lazy functional
language Haskell. It is based on the concept of observation of
intermediate data structures, rather than the more traditional
stepping and variable examination paradigm used by imperative
language debuggers.

Copyright (c) Andy Gill, 1992-2000
Copyright (c) The University of Kansas 2010
Copyright (c) Maarten Faddegon, 2013-2014

All rights reserved. HOOD is distributed as free software under
the license in the file "License", which available from the HOOD
web page, http://www.haskell.org/hood

This module produces CDS's, based on the observation made on Haskell
objects, including base types, constructors and functions.

WARNING: unrestricted use of unsafePerformIO below.

This was ported for the version found on www.haskell.org/hood.


%************************************************************************
%*									*
\subsection{Exports}
%*									*
%************************************************************************

\begin{code}
module Debug.Hoed.Observe
  (
   -- * The main Hood API
  
     observe
  , gdmobserve
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
  -- , Observing      -- a -> a
  , Observable(..) -- Class
  , runO	   -- IO a -> IO ()
  , printO	   -- a -> IO ()
  , putStrO	   -- String -> IO ()

   -- * For advanced users, that want to render their own datatypes.
  , (<<)           -- (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
  , thunk          -- (Observable a) => a -> ObserverM a	
  , nothunk
  , send
  , observeBase
  , observeOpaque

  , observedTypes

  -- * For users that want to write there own render drivers.
  
  , debugO	   -- IO a -> IO [CDS]
  , CDS(..)

  , Generic
  ) where	
\end{code}


%************************************************************************
%*									*
\subsection{Imports and infixing}
%*									*
%************************************************************************

\begin{code}
import System.IO
import Data.Maybe
import Control.Monad
import Data.Array as Array
import Data.List
import Data.Char
import System.Environment

import Language.Haskell.TH
import GHC.Generics

-- The only non standard one we assume
--import IOExts
import Data.IORef
import System.IO.Unsafe
\end{code}

\begin{code}
import Control.Concurrent
\end{code}

Needed to access the cost centre stack:
\begin{code}
import GHC.Stack (ccLabel, getCurrentCCS, CostCentreStack,ccsCC,ccsParent,currentCallStack)
import GHC.Foreign as GHC
import GHC.Ptr
\end{code}

Library with our graph algorithms:
\begin{code}
import Data.Graph.Libgraph
\end{code}

User interface imports:
\begin{code}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny (startGUI,defaultConfig,tpPort,tpStatic
                              , Window, UI, (#), (#+), (#.), string, on
                              )
import System.Process(system)
\end{code}


\begin{code}
import Control.Exception ( Exception, throw )
import qualified Control.Exception as Exception
{-
 ( catch
		, Exception(..)
		, throw
		) as Exception
-}
import Data.Dynamic ( Dynamic )
\end{code}

\begin{code}
infixl 9 <<
\end{code}


%************************************************************************
%*									*
\subsection{External start functions}
%*									*
%************************************************************************

Run the observe ridden code.

\begin{code}
-- | run some code and return the CDS structure (for when you want to write your own debugger).
debugO :: IO a -> IO [CDS]
debugO program = 
     do { initUniq
	; startEventStream
        ; let errorMsg e = "[Escaping Exception in Code : " ++ show e ++ "]"
	; ourCatchAllIO (do { program ; return () }) 
			(hPutStrLn stderr . errorMsg)
        ; events <- endEventStream
	; return (eventsToCDS events)
	}

-- | print a value, with debugging 
printO :: (Show a) => a -> IO ()
printO expr = runO [] (print expr)

-- | print a string, with debugging 
putStrO :: String -> IO ()
putStrO expr = runO [] (putStr expr)

-- | The main entry point; run some IO code, and debug inside it.
-- 
-- An example of using this debugger is 
--
-- @runO (print [ observe "+1" (+1) x | x <- observe "xs" [1..3]])@
-- 
-- @[2,3,4]
-- -- +1
--  { \ 1  -> 2
--  }
-- -- +1
--  { \ 2  -> 3
--  }
-- -- +1
--  { \ 3  -> 4
--  }
-- -- xs
--  1 : 2 : 3 : []@
-- 
-- Which says, the return is @[2,3,4]@, there were @3@ calls to +1
-- (showing arguments and results), and @xs@, which was the list
-- @1 : 2 : 3 : []@.
-- 

runO :: [(String,String)] -> IO a -> IO ()
runO slices program =
    do { args <- getArgs
       ; setPushMode (parseArgs args)
       ; hPutStrLn stderr "=== program output ===\n"
       ; cdss <- debugO program
       ; let cdss1 = rmEntrySet cdss
       ; let cdss2 = simplifyCDSSet cdss1

       ; let eqs   = ((sortBy byStack) . renderEquations) cdss2
       ; hPutStrLn stderr "\n===\n"
       ; hPutStrLn stderr (showWithStack eqs)
       ; let compGraph = mkGraph eqs
       ; debugSession slices compGraph
       ; return ()
       }

hPutStrList :: (Show a) => Handle -> [a] -> IO()
hPutStrList h []     = hPutStrLn h ""
hPutStrList h (c:cs) = do {hPutStrLn h (show c); hPutStrList h cs}


------------------------------------------------------------------------
-- Push mode option handling

data PushMode = Vanilla | Drop | Truncate

pushMode :: IORef PushMode
pushMode = unsafePerformIO $ newIORef Vanilla

setPushMode :: PushMode -> IO ()
setPushMode = writeIORef pushMode

getPushMode :: PushMode
getPushMode = unsafePerformIO $ readIORef pushMode

-- MF TODO: handle a bit nicer?
parseArgs :: [String] -> PushMode
parseArgs []      = Truncate -- default mode
parseArgs (arg:_) = case arg of
        "--PushVanilla"  -> Vanilla
        "--PushDrop"     -> Drop
        "--PushTruncate" -> Truncate
        _              -> error ("unknown option " ++ arg)

------------------------------------------------------------------------
-- The Equation type

data Equation = Equation {equLabel :: String, equRes :: String, equStack :: CallStack}
                deriving (Eq, Ord)

instance Show Equation where
  show e = equRes e -- ++ " with stack " ++ show (equStack e)
  showList eqs eq = unlines (map show eqs) ++ eq

showWithStack :: [Equation] -> String
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

nextStack = case getPushMode of
        Vanilla  -> nextStack_vanilla
        Drop     -> nextStack_drop
        Truncate -> nextStack_truncate

-- Always push onto top of stack
nextStack_vanilla :: Equation -> CallStack
nextStack_vanilla (Equation cc _ ccs) = cc:ccs

-- Drop on recursion
nextStack_drop :: Equation -> CallStack
nextStack_drop (Equation cc _ [])   = [cc]
nextStack_drop (Equation cc _ ccs)
  = if ccs `contains` cc 
        then ccs
        else cc:ccs

-- Remove everything between recursion (e.g. [f,g,f,h] becomes [f,h])
nextStack_truncate :: Equation -> CallStack
nextStack_truncate (Equation cc _ [])   = [cc]
nextStack_truncate (Equation cc _ ccs)
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
-- computation graphs

data Vertex = Vertex {equations :: [Equation], status :: Status}
              deriving (Eq,Show)

data Dependency = PushDep | CallDep Int deriving (Eq,Show)

type CompGraph = Graph Vertex Dependency

mkGraph :: [Equation] -> CompGraph
mkGraph trc = mapGraph (\r->Vertex [r] Unassessed) (mkGraph' trc)

mkGraph' :: [Equation] -> Graph Equation Dependency
mkGraph' trc = Graph (head roots)
                       trc
                       (foldr (\r as -> as ++ (arcsFrom r trc)) [] trc)
  where roots = filter (\equ -> equStack equ == []) trc

arcsFrom :: Equation -> [Equation] -> [Arc Equation Dependency]
arcsFrom src trc
  =  ((map (\tgt -> Arc src tgt PushDep)) . (filter isPushArc) $ trc)
  ++ ((map (\tgt -> Arc src tgt (CallDep 1))) . (filter isCall1Arc) $ trc)

  where isPushArc = pushDependency src
        
        isCall1Arc = anyOf $ map (flip callDependency src) trc

        anyOf :: [a->Bool] -> a -> Bool
        anyOf ps x = or (map (\p -> p x) ps)

pushDependency :: Equation -> Equation -> Bool
pushDependency p c
  = nextStack p == equStack c

callDependency :: Equation -> Equation -> Equation -> Bool
callDependency pApp pLam c 
  = call (nextStack pApp) (nextStack pLam) == equStack c

------------------------------------------------------------------------
-- Algorithmic Debugging

data Status = Correct | Wrong | Unassessed
              deriving (Show, Eq, Ord)

debugSession :: [(String,String)] -> CompGraph -> IO ()
debugSession slices tree
  = do treeRef <- newIORef tree
       startGUI defaultConfig
           { tpPort       = Just 10000
           , tpStatic     = Just "./wwwroot"
           } (debugSession' slices treeRef)

preorder :: CompGraph -> [Vertex]
preorder = getPreorder . getDfs

debugSession' :: [(String,String)] -> IORef CompGraph -> Window -> UI ()
debugSession' sliceDict treeRef window
  = do return window # UI.set UI.title "Hoed debugging session"
       UI.addStyleSheet window "debug.css"
       img <- UI.img 
       redraw img treeRef
       buttons <- UI.div #. "buttons"
       nowrap  <- UI.div #. "nowrap"  #+ (map UI.element [buttons,img])
       UI.getBody window #+ [UI.element nowrap]

       tree <- UI.liftIO $ readIORef treeRef
       let ns = (preorder tree)
       ts <- toElems sliceDict ns
       ds <- mapM (uncurry divpack) (zip ts (cycle [Odd,Even]))
       UI.element buttons # UI.set UI.children ds
       mapM_ (onClick buttons img treeRef Correct) 
             (zip (corButtons ts) (reverse ns))
       mapM_ (onClick buttons img treeRef Wrong)
             (zip (wrnButtons ts) (reverse ns))


-- addButtons buttons = do



--              Slice      Hr         Equation   Correct    Wrong
type ElemSet = (UI.Element,UI.Element,UI.Element,UI.Element,UI.Element)

data OddEven = Odd | Even

divpack :: ElemSet -> OddEven -> UI UI.Element
divpack (e1,e2,e3,e4,e5) x
  = UI.div #. lbl x #+ map UI.element [e1,e2,e3,e4,e5]
    where lbl Odd  = "odd"
          lbl Even = "even"

onClick :: UI.Element -> UI.Element -> IORef CompGraph -> Status 
        -> (UI.Element,Vertex) -> UI ()
onClick buttons img treeRef status (b,n) 
  = do on UI.click b $ \_ -> do 
        updateTree img treeRef (\tree -> markNode tree n status)
        -- UI.element b # UI.set UI.text "I have been clicked!"
        -- UI.element buttons # UI.set UI.children []
        

markNode :: CompGraph -> Vertex -> Status -> CompGraph
markNode g v s = mapGraph (\v' -> if v' == v then v{status=s} else v') g

corButtons :: [ElemSet] -> [UI.Element]
corButtons = foldl (\es (_,_,_,e,_) -> e : es) []

wrnButtons :: [ElemSet] -> [UI.Element]
wrnButtons = foldl (\es (_,_,_,_,e) -> e : es) []

toElems :: [(String,String)] -> [Vertex] -> UI [ElemSet]
toElems sliceDict xs = mapM (toElem sliceDict) xs

toElem :: [(String,String)] -> Vertex -> UI ElemSet
toElem sliceDict v 
  = do slc <- UI.pre    # UI.set UI.text (foldl (\acc e -> acc ++ getSlice e) "" $ equations v)
       hr  <- UI.hr
       shw <- UI.pre    # UI.set UI.text (foldl (\acc e -> acc ++ show e ++ "\n") "" $ equations v)
       cor <- UI.button # UI.set UI.text "right"
       wrg <- UI.button # UI.set UI.text "wrong"
       return (slc,hr,shw,cor,wrg)
   where getSlice e = case equLabel e `lookup` sliceDict of
              Nothing -> ""
              Just s  -> s

updateTree :: UI.Element -> IORef CompGraph -> (CompGraph -> CompGraph)
           -> UI ()
updateTree img treeRef f
  = do tree <- UI.liftIO $ readIORef treeRef
       UI.liftIO $ writeIORef treeRef (f tree)
       redraw img treeRef

redraw :: UI.Element -> IORef CompGraph -> UI ()
redraw img treeRef 
  = do tree <- UI.liftIO $ readIORef treeRef
       UI.liftIO $ writeFile "debugTree.dot" (shw tree)
       UI.liftIO $ system $ "dot -Tpng -Gsize=7,8 -Gdpi=100 debugTree.dot "
                          ++ "> wwwroot/debugTree.png"
       url <- UI.loadFile "image/png" "wwwroot/debugTree.png"
       UI.element img # UI.set UI.src url
       return ()

  where shw g = showWith g showVertex showArc
        showVertex :: Vertex -> String
        showVertex v = (show . status) v ++ "\n" ++ showEquations v
        showEquations = showEquations' . equations
        showEquations' [e] = show e
        showEquations' es  = foldl (\acc e-> acc ++ show e ++ ", ") "{" (init es) 
                             ++ show (last es) ++ "}"

        -- showVertex = show
        -- showVertex = (foldl (++) "") . (map show) . equations
        showArc _  = ""

------------------------------------------------------------------------
-- Render equations from CDS set

renderEquations :: CDSSet -> [Equation]
renderEquations = map renderEquation

renderEquation :: CDS -> Equation
renderEquation (CDSNamed name set)
  = Equation name equation (head stack)                    -- MF TODO: head?
  where equation    =  pretty 40 (foldr (<>) nil doc)
        (doc,stack) = unzip rendered
        rendered    = map (renderNamedTop name) output
        output      = cdssToOutput set
        -- MF TODO: Do we want to sort?
        -- output      = (commonOutput . cdssToOutput) set
renderEquation _ = Equation "??" "??" emptyStack

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

\end{code}


%************************************************************************
%*									*
\subsection{Simulations}
%*									*
%************************************************************************

Here we provide stubs for the functionally that is not supported
by some compilers, and provide some combinators of various flavors.

\begin{code}
ourCatchAllIO :: IO a -> (Exception.SomeException -> IO a) -> IO a
ourCatchAllIO = Exception.catch

handleExc :: Parent -> Exception.SomeException -> IO a
handleExc context exc = return (send "throw" (return throw << exc) context)
\end{code}


%************************************************************************
%*									*
\subsection{GDM Generics}
%*									*
%************************************************************************

he generic implementation of the observer function.

\begin{code}
class Observable a where
	observer  :: a -> Parent -> a 
        default observer :: (Generic a, GObservable (Rep a)) => a -> Parent -> a
        observer x c = to (gdmobserver (from x) c)

class GObservable f where
        gdmobserver :: f a -> Parent -> f a
        gdmObserveChildren :: f a -> ObserverM (f a)
        gdmShallowShow :: f a -> String
\end{code}

Creating a shallow representation for types of the Data class.

\begin{code}

-- shallowShow :: Constructor c => t c (f :: * -> *) a -> [Char]
-- shallowShow = conName

\end{code}

Observing the children of Data types of kind *.

\begin{code}

-- Meta: data types
instance (GObservable a) => GObservable (M1 D d a) where
        gdmobserver m@(M1 x) cxt = M1 (gdmobserver x cxt)
        gdmObserveChildren = gthunk
        gdmShallowShow = undefined

-- Meta: Constructors
instance (GObservable a, Constructor c) => GObservable (M1 C c a) where
        gdmobserver m@(M1 x) cxt = M1 (send (gdmShallowShow m) (gdmObserveChildren x) cxt)
        gdmObserveChildren = gthunk
        gdmShallowShow = conName

-- Meta: Selectors
--      | selName m == "" = M1 y
--      | otherwise       = M1 (send (selName m) (return y) cxt)
instance (GObservable a, Selector s) => GObservable (M1 S s a) where
        gdmobserver m@(M1 x) cxt
          | selName m == "" = M1 (gdmobserver x cxt)
          | otherwise       = M1 (send (selName m ++ " =") (gdmObserveChildren x) cxt)
        gdmObserveChildren = gthunk
        gdmShallowShow = undefined

-- Unit: used for constructors without arguments
instance GObservable U1 where
        gdmobserver x _ = x
        gdmObserveChildren = return
        gdmShallowShow = undefined

-- Products: encode multiple arguments to constructors
instance (GObservable a, GObservable b) => GObservable (a :*: b) where
        gdmobserver (a :*: b) cxt = error "gdmobserver product"

        gdmObserveChildren (a :*: b) = do a'  <- gdmObserveChildren a
                                          b'  <- gdmObserveChildren b
                                          return (a' :*: b')
                                       
        gdmShallowShow = undefined

-- Sums: encode choice between constructors
instance (GObservable a, GObservable b) => GObservable (a :+: b) where
        gdmobserver (L1 x) cxt = L1 (gdmobserver x cxt)
        gdmobserver (R1 x) cxt = R1 (gdmobserver x cxt)

        gdmObserveChildren (R1 x) = do {x' <- gdmObserveChildren x; return (R1 x')}
        gdmObserveChildren (L1 x) = do {x' <- gdmObserveChildren x; return (L1 x')}

        gdmShallowShow = undefined

-- Constants: additional parameters and recursion of kind *
instance (Observable a) => GObservable (K1 i a) where
        gdmobserver (K1 x) cxt = K1 (observer_ observer x cxt)

        gdmObserveChildren = gthunk

        gdmShallowShow = undefined
\end{code}

Observing functions is done via the ad-hoc mechanism, because
we provide an instance definition the default is ignored for
this type.

\begin{code}
instance (Observable a,Observable b) => Observable (a -> b) where
  observer fn cxt arg = gdmFunObserver cxt fn arg
\end{code}

Observing the children of Data types of kind *->*.

\begin{code}
gdmFunObserver :: (Observable a,Observable b) => Parent -> (a->b) -> (a->b)
gdmFunObserver cxt fn arg
        = let (app,stack) = getStack 
                          $ sendObserveFnPacket stack
                            (do arg' <- thunk observer arg
                                thunk observer (fn arg')
                            ) cxt
          in app
\end{code}




%************************************************************************
%*									*
\subsection{Cost Centre Stack}
%*									*
%************************************************************************

\begin{code}

{-# NOINLINE getStack #-}
getStack :: a -> (a, CallStack)
getStack x = let stack = unsafePerformIO 
                         $ do {ccs <- getCurrentCCS (); ccsToStrings ccs}
             in  (x, rev stack)
        where rev []    = error "empty stack"
              rev s = reverse (tail s)

ccsToStrings :: Ptr CostCentreStack -> IO [String]
ccsToStrings ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs == nullPtr = return acc
     | otherwise = do
        cc  <- ccsCC ccs
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        parent <- ccsParent ccs
        if (lbl == "MAIN")
           then return acc
           else go parent (lbl : acc)


\end{code}


%************************************************************************
%*									*
\subsection{Generics}
%*									*
%************************************************************************

Generate a new observe from generated observers and the gobserve mechanism.
Where gobserve is the 'classic' observe but parametrized.

\begin{code}
observe :: String -> Q Exp
observe s = do n  <- methodName s
               let f  = return $ VarE n
                   s' = stringE s
               [| (\x-> (gobserve $f $s' x)) |]
\end{code}

Generate class definition and class instances for list of types.

\begin{code}
observedTypes :: String -> [Q Type] -> Q [Dec]
observedTypes s qt = do cd <- (genClassDef s)
                        ci <- foldM f [] qt
                        bi <- foldM g [] baseTypes
                        fi <- (gfunObserver s)
                        -- li <- (gListObserver s) MF TODO: should we do away with these?
                        return (cd ++ ci ++ bi ++ fi)
        where f d t = do ds <- (gobservableInstance s t)
                         return (ds ++ d)
              g d t = do ds <- (gobservableBaseInstance s t)
                         return (ds ++ d)
              baseTypes = [[t|Int|], [t|Char|], [t|Float|], [t|Bool|]]



\end{code}

Generate a class definition from a string

\begin{code}

genClassDef :: String -> Q [Dec]
genClassDef s = do cn <- className s
                   mn <- methodName s
                   nn <-  newName "a"
                   let a   = PlainTV nn
                       tvb = [a]
                       vt  = varT nn
                   mt <- [t| $vt -> Parent -> $vt |]
                   let m   = SigD mn mt
                       cd  = ClassD [] cn tvb [] [m]
                   return [cd]

className :: String -> Q Name
className s = return $ mkName ("Observable" ++ headToUpper s)

methodName :: String -> Q Name
methodName s = return $ mkName ("observer" ++ headToUpper s)

headToUpper (c:cs) = toUpper c : cs

\end{code}

\begin{code}
gobserverBase :: Q Name -> Q Type -> Q [Dec]
gobserverBase qn t = do n <- qn
                        c <- gobserverBaseClause qn
                        return [FunD n [c]]

gobserverBaseClause :: Q Name -> Q Clause
gobserverBaseClause qn = clause [] (normalB (varE $ mkName "observeBase")) []

gobserverList :: Q Name -> Q [Dec]
gobserverList qn = do n  <- qn
                      cs <-listClauses qn
                      return [FunD n cs]


\end{code}

The generic implementation of the observer function, special cases
for base types and functions.

\begin{code}
gobserver :: Q Name -> Q Type -> Q [Dec]
gobserver qn t = do n <- qn
                    cs <- gobserverClauses qn t
                    return [FunD n cs]

gobserverClauses :: Q Name -> Q Type -> Q [Clause]
gobserverClauses n qt = do t  <- qt
                           bs <- getBindings qt
                           case t of
                                _     -> do cs <- (getConstructors . getName) qt
                                            mapM (gobserverClause t n bs) cs

gobserverClause :: Type -> Q Name -> TyVarMap -> Con -> Q Clause
gobserverClause t n bs (y@(NormalC name fields))
  = do { vars <- guniqueVariables (length fields)
       ; let evars = map varE vars
             pvars = map varP vars
             c'    = varP (mkName "c")
             c     = varE (mkName "c")
       ; clause [conP name pvars, c']
           ( normalB [| send $(shallowShow y) $(observeChildren n t bs y evars) $c |]
           ) []
       }
gobserverClause t n bs (InfixC left name right) 
  = gobserverClause t n bs (NormalC name (left:[right]))
gobserverClause t n bs y = error ("gobserverClause can't handle " ++ show y)

listClauses :: Q Name -> Q [Clause]
listClauses n = do l1 <- listClause1 n 
                   l2 <- listClause2 n 
                   return [l1, l2]

-- observer (a:as) = send ":"  (return (:) << a << as)
listClause1 :: Q Name -> Q Clause
listClause1 qn
  = do { n <- qn
       ; let a'    = varP (mkName "a")
             a     = varE (mkName "a")
             as'   = varP (mkName "as")
             as    = varE (mkName "as") 
             c'    = varP (mkName "c")
             c     = varE (mkName "c")
             t     = [| thunk $(varE n)|] -- MF TODO: or nothunk
             name  = mkName ":"
       ; clause [infixP a' name as', c']
           ( normalB [| send ":" ( compositionM $t
                                   ( compositionM $t
                                     ( return (:)
                                     ) $a
                                   ) $as
                                 ) $c
                     |]
           ) []
       }

-- observer []     = send "[]" (return [])
listClause2 :: Q Name -> Q Clause
listClause2 qn
  = do { n <- qn
       ; let c'    = varP (mkName "c")
             c     = varE (mkName "c")
       ; clause [wildP, c']
           ( normalB [| send "[]" (return []) $c |]
           ) []
       }

\end{code}

We also need to do some work to also generate the instance declaration
around the observer method.

\begin{code}
gobservableInstance :: String -> Q Type -> Q [Dec]
gobservableInstance s qt 
  = do t  <- qt
       cn <- className s
       let ct = conT cn
       n  <- case t of
            (ForallT tvs _ t') -> [t| $ct $(return t') |]
            _                  -> [t| $ct $qt          |]
       m  <- gobserver (methodName s) qt
       c  <- case t of 
                (ForallT _ c' _)   -> return c'
                _                  -> return []
       return [InstanceD (updateContext cn c) n m]

updateContext :: Name -> [Pred] -> [Pred]
updateContext cn ps = map f ps
        where f (ClassP n ts)
                | nameBase n == "Observable" = ClassP cn ts
                | otherwise                  = ClassP n  ts
              f p = p

gobservableBaseInstance :: String -> Q Type -> Q [Dec]
gobservableBaseInstance s qt
  = do t  <- qt
       cn <- className s
       let ct = conT cn
       n  <- case t of
            (ForallT tvs _ t') -> [t| $ct $(return t') |]
            _                  -> [t| $ct $qt          |]
       m  <- gobserverBase (methodName s) qt
       c  <- case t of 
                (ForallT _ c' _)   -> return c'
                _                  -> return []
       return [InstanceD c n m]

gobservableListInstance :: String -> Q [Dec]
gobservableListInstance s
  = do let qt = [t|forall a . [] a |]
       t  <- qt
       cn <- className s
       let ct = conT cn
       n  <- case t of
            (ForallT tvs _ t') -> [t| $ct $(return t') |]
            _                  -> [t| $ct $qt          |]
       m  <- gobserverList (methodName s)
       c  <- case t of 
                (ForallT _ c' _)   -> return c'
                _                  -> return []
       return [InstanceD c n m]

-- MF TODO: what do we do with this?
-- gListObserver :: String -> Q [Dec]
-- gListObserver s
--   = do cn <- className s
--        let ct = conT cn
--            a  = VarT (mkName "a")
--            a' = return a
--        c <- return [ClassP cn a']
--        n <- [t| $ct [$a'] |]
--        m <- gobserverList (methodName s)
--        return [InstanceD c n m]


gobserverFunClause :: Name -> Q Clause
gobserverFunClause n
  = do { [f',a'] <- guniqueVariables 2
       ; let vs        = [f', mkName "c", a']
             [f, c, a] = map varE vs
             pvars     = map varP vs
       ; clause pvars 
         (normalB [| let (app,stack) = getStack 
                                     $ sendObserveFnPacket stack
                                       ( do a' <- thunk $(varE n) $a
                                            thunk $(varE n) ($f a')
                                       ) $c
                     in app
                  |]
         ) []
       }

gobserverFun :: Q Name -> Q [Dec]
gobserverFun qn
  = do n  <- qn
       c  <- gobserverFunClause n
       cs <- return [c]
       return [FunD n cs]

gfunObserver :: String -> Q [Dec]
gfunObserver s
  = do cn <- className s
       let ct = conT cn
           a  = VarT (mkName "a")
           b  = VarT (mkName "b")
           f  = return $ AppT (AppT ArrowT a) b
           a' = return a
           b' = return b
       p <- return $ ClassP cn a'
       q <- return $ ClassP cn b'
       c <- return [p,q]
       n <- [t| $ct $f |]
       m <- gobserverFun (methodName s)
       return [InstanceD c n m]

\end{code}

Creating a shallow representation for types of the Data class.

\begin{code}
shallowShow :: Con -> ExpQ
shallowShow (NormalC name _)
  = stringE (case (nameBase name) of "(,)" -> ","; s -> s)
\end{code}

Observing the children of Data types of kind *.

Note how we are forced to add the extra 'vars' argument that should
have the same unique name as the corresponding pattern.

To implement observeChildren we also define a mapM and compositionM function.
To our knowledge there is no existing work that do this in a generic fashion
with Template Haskell.

\begin{code}

isObservable :: TyVarMap -> Type -> Type -> Q Bool
-- MF TODO: if s == t then return True else isObservable' bs t
isObservable bs s t = isObservable' bs t

-- MF TODO this is a hack
isObservable' bs (AppT ListT _)    = return True

isObservable' bs (VarT n)      = case lookupBinding bs n of
                                      (Just (T t)) -> isObservableT t
                                      (Just (P p)) -> isObservableP p
                                      Nothing      -> return False
-- isObservable' bs (AppT t _)    = isObservable' bs t
isObservable' (n,_) t@(ConT m) = if n == m then return True else isObservableT t
isObservable' bs t             = isObservableT t

isObservableT :: Type -> Q Bool
isObservableT t@(ConT _) = isInstance (mkName "Observable") [t]
isObservableT _          = return False 

isObservableP :: Pred -> Q Bool
isObservableP (ClassP n _) = return $ (nameBase n) == "Observable"
isObservableP _            = return False


thunkObservable :: Q Name -> TyVarMap -> Type -> Type -> Q Exp
thunkObservable qn bs s t
  = do i <- isObservable bs s t
       n <- qn
       if i then [| thunk $(varE n) |] else [| nothunk |]

observeChildren :: Q Name -> Type -> TyVarMap -> Con -> [Q Exp] -> Q Exp
observeChildren n t bs = gmapM (thunkObservable n bs t)

gmapM :: (Type -> Q Exp) -> Con -> [ExpQ] -> ExpQ
gmapM f (NormalC name fields) vars
  = m name (reverse fields) (reverse vars) 
  where m :: Name -> [(Strict,Type)] -> [ExpQ] -> ExpQ
        m n _      []           = [| return $(conE n)                      |]
        m n ((_,t):ts) (v:vars) = [| compositionM $(f t) $(m n ts vars) $v |]


compositionM :: Monad m => (a -> m b) -> m (b -> c) -> a -> m c
compositionM f g x = do { g' <- g 
                        ; x' <- f x 
                        ; return (g' x') 
                        }
\end{code}

And we need some helper functions:

\begin{code}

-- A mapping from typevars to the type they are bound to.

type TyVarMap = (Name, [(TyVarBndr,TypeOrPred)])

data TypeOrPred = T Type | P Pred


-- MF TODO lookupBinding

lookupBinding :: TyVarMap -> Name -> Maybe TypeOrPred
lookupBinding (_,[]) _ = Nothing
lookupBinding (r,((b,t):ts)) n
  = let m = case b of (PlainTV  m  ) -> m
                      (KindedTV m _) ->m
    in if (m == n) then Just t else lookupBinding (r,ts) n

-- Given a parametrized type, get a list with typevars and their bindings
-- e.g. [(a,Int), (b,Float)] in (MyData a b) Int Float

getBindings :: Q Type -> Q TyVarMap
getBindings t = do bs  <- getBs t
                   tvs <- (getTvbs . getName) t
                   pbs <- getPBindings t
                   n   <- getName t
                   let fromApps = (zip tvs (map T bs))
                       fromCxt  = (zip tvs (map P pbs)) 
                   return (n, (fromCxt ++ fromApps))

getPBindings :: Q Type -> Q [Pred]
getPBindings qt = do t <- qt 
                     case t of (ForallT _ cs _) -> getPBindings' cs
                               _                -> return []

getPBindings' :: [Pred] -> Q [Pred]
getPBindings' []     = return []
getPBindings' (p:ps) = do pbs <- getPBindings' ps
                          return $ case p of (ClassP n t)   -> p : pbs
                                             _            -> pbs

-- Given a parametrized type, get a list with its type variables
-- e.g. [a,b] in (MyData a b) Int Float

getTvbs :: Q Name -> Q [TyVarBndr]
getTvbs name = do n <- name
                  i <- reify n
                  case i of
                    TyConI (DataD _ _ tvbs _ _) 
                      -> return tvbs
                    i
                      -> error ("getTvbs: can't reify " ++ show i)

-- Given a parametrized type, get a list with the bindings of type variables
-- e.g. [Int,Float] in (MyData a b) Int Float

getBs :: Q Type -> Q [Type]
getBs t = do t' <- t
             let t'' = case t' of (ForallT _ _ s) -> s
                                  _               -> t'
             return (getBs' t'')

getBs' :: Type -> [Type]
getBs' (AppT c t) = t : getBs' c
getBs' _          = []

-- Given a parametrized type, get the name of the type constructor (e.g. Tree in Tree Int)

getName :: Q Type -> Q Name
getName t = do t' <- t
               getName' t'

getName' :: Type -> Q Name
getName' t = case t of 
      		(ForallT _ _ t'') -> getName' t''
                (AppT t'' _)      -> getName' t''
      		(ConT name)       -> return name
                ListT             -> return $ mkName "[]"
                TupleT _          -> return $ mkName "(,)"
                t''               -> error ("getName can't handle " ++ show t'')

-- Given a type, get a list of type variables.

getTvs :: Q Type -> Q [TyVarBndr]
getTvs t = do {(ForallT tvs _ _) <- t; return tvs }

-- Given a type, get a list of constructors.

getConstructors :: Q Name -> Q [Con]
getConstructors name = do {n <- name; TyConI (DataD _ _ _ cs _)  <- reify n; return cs}

guniqueVariables :: Int -> Q [Name]
guniqueVariables n = replicateM n (newName "x")

observableCxt :: [TyVarBndr] -> Q Cxt
observableCxt tvs = return [classpObservable $ map (\v -> (tvname v)) tvs]

classpObservable :: [Type] -> Pred
classpObservable = ClassP (mkName "Observable")

qcontObservable :: Q Type
qcontObservable = return contObservable

contObservable :: Type
contObservable = ConT (mkName "Observable")

qtvname :: TyVarBndr -> Q Type
qtvname = return . tvname

tvname :: TyVarBndr -> Type
tvname (PlainTV  name  ) = VarT name
tvname (KindedTV name _) = VarT name

\end{code}

%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

 The Haskell Base types

\begin{code}
instance Observable Int 	where { observer = observeBase }
instance Observable Bool 	where { observer = observeBase }
instance Observable Integer 	where { observer = observeBase }
instance Observable Float 	where { observer = observeBase }
instance Observable Double	where { observer = observeBase }
instance Observable Char 	where { observer = observeBase }

instance Observable ()		where { observer = observeOpaque "()" }

-- utilities for base types.
-- The strictness (by using seq) is the same 
-- as the pattern matching done on other constructors.
-- we evalute to WHNF, and not further.

observeBase :: (Show a) => a -> Parent -> a
observeBase lit cxt = seq lit $ send (show lit) (return lit) cxt

observeOpaque :: String -> a -> Parent -> a
observeOpaque str val cxt = seq val $ send str (return val) cxt
\end{code}

The Constructors.

\begin{code}
instance (Observable a,Observable b) => Observable (a,b) where
  observer (a,b) = send "," (return (,) << a << b)

instance (Observable a,Observable b,Observable c) => Observable (a,b,c) where
  observer (a,b,c) = send "," (return (,,) << a << b << c)

instance (Observable a,Observable b,Observable c,Observable d) 
	  => Observable (a,b,c,d) where
  observer (a,b,c,d) = send "," (return (,,,) << a << b << c << d)

instance (Observable a,Observable b,Observable c,Observable d,Observable e) 
	 => Observable (a,b,c,d,e) where
  observer (a,b,c,d,e) = send "," (return (,,,,) << a << b << c << d << e)

instance (Observable a) => Observable [a] where
  observer (a:as) = send ":"  (return (:) << a << as)
  observer []     = send "[]" (return [])

instance (Observable a) => Observable (Maybe a) where
  observer (Just a) = send "Just"    (return Just << a)
  observer Nothing  = send "Nothing" (return Nothing)

instance (Observable a,Observable b) => Observable (Either a b) where
  observer (Left a)  = send "Left"  (return Left  << a)
  observer (Right a) = send "Right" (return Right << a)
\end{code}

Arrays.

\begin{code}
instance (Ix a,Observable a,Observable b) => Observable (Array.Array a b) where
  observer arr = send "array" (return Array.array << Array.bounds arr 
					          << Array.assocs arr
			      )
\end{code}

IO monad.

\begin{code}
instance (Observable a) => Observable (IO a) where
  observer fn cxt = 
	do res <- fn
	   send "<IO>" (return return << res) cxt
\end{code}



The Exception *datatype* (not exceptions themselves!).
For now, we only display IOExceptions and calls to Error.

\begin{code}
instance Observable Exception.SomeException where
--  observer (IOException a)      = observeOpaque "IOException" (IOException a)
--  observer (ErrorCall a)        = send "ErrorCall"   (return ErrorCall << a)
  observer other                = send "<Exception>" (return other)

instance Observable Dynamic where { observer = observeOpaque "<Dynamic>" }
\end{code}


%************************************************************************
%*									*
\subsection{Classes and Data Definitions}
%*									*
%************************************************************************

\begin{code}
type Observing a = a -> a
\end{code}

MF: when do we need this type?

\begin{code}
newtype Observer = O (forall a . (Observable a) => String -> a -> a)

defaultObservers :: (Observable a) => String -> (Observer -> a) -> a
defaultObservers label fn = unsafeWithUniq $ \ node ->
     do { sendEvent node (Parent 0 0) (Observe label)
	; let observe' sublabel a
	       = unsafeWithUniq $ \ subnode ->
		 do { sendEvent subnode (Parent node 0) 
		                        (Observe sublabel)
		    ; return (observer_ observer a (Parent
			{ observeParent = subnode
			, observePort   = 0
		        }))
		    }
        ; return (observer_ observer (fn (O observe'))
		       (Parent
			{ observeParent = node
			, observePort   = 0
		        }))
	}
defaultFnObservers :: (Observable a, Observable b) 
		      => String -> (Observer -> a -> b) -> a -> b
defaultFnObservers label fn arg = unsafeWithUniq $ \ node ->
     do { sendEvent node (Parent 0 0) (Observe label)
	; let observe' sublabel a
	       = unsafeWithUniq $ \ subnode ->
		 do { sendEvent subnode (Parent node 0) 
		                        (Observe sublabel)
		    ; return (observer_ observer a (Parent
			{ observeParent = subnode
			, observePort   = 0
		        }))
		    }
        ; return (observer_ observer (fn (O observe'))
		       (Parent
			{ observeParent = node
			, observePort   = 0
		        }) arg)
	}
\end{code}


%************************************************************************
%*									*
\subsection{The ObserveM Monad}
%*									*
%************************************************************************

The Observer monad, a simple state monad, 
for placing numbers on sub-observations.

\begin{code}
newtype ObserverM a = ObserverM { runMO :: Int -> Int -> (a,Int) }

instance Monad ObserverM where
	return a = ObserverM (\ c i -> (a,i))
	fn >>= k = ObserverM (\ c i ->
		case runMO fn c i of
		  (r,i2) -> runMO (k r) c i2
		)

thunk :: (a -> Parent -> a) -> a -> ObserverM a
thunk f a = ObserverM $ \ parent port ->
		( observer_ f a (Parent
				{ observeParent = parent
				, observePort   = port
				}) 
		, port+1 )

gthunk :: (GObservable f) => f a -> ObserverM (f a)
gthunk a = ObserverM $ \ parent port ->
		( gdmobserver_ a (Parent
				{ observeParent = parent
				, observePort   = port
				}) 
		, port+1 )

nothunk :: a -> ObserverM a
nothunk a = ObserverM $ \ parent port ->
		( observer__ a (Parent
				{ observeParent = parent
				, observePort   = port
				}) 
		, port+1 )


(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
-- fn << a = do { fn' <- fn ; a' <- thunk a ; return (fn' a') }
fn << a = gdMapM (thunk observer) fn a

gdMapM :: (Monad m)
        => (a -> m a)  -- f
        -> m (a -> b)  -- data constructor
        -> a           -- argument
        -> m b         -- data
gdMapM f c a = do { c' <- c ; a' <- f a ; return (c' a') }

\end{code}


%************************************************************************
%*									*
\subsection{observe and friends}
%*									*
%************************************************************************

Our principle function and class

\begin{code}
-- | 'observe' observes data structures in flight.
--  
-- An example of use is 
--  @
--    map (+1) . observe \"intermeduate\" . map (+2)
--  @
--
-- In this example, we observe the value that flows from the producer
-- @map (+2)@ to the consumer @map (+1)@.
-- 
-- 'observe' can also observe functions as well a structural values.
-- 
{-# NOINLINE gobserve #-}
gobserve :: (a->Parent->a) -> String -> a -> a
gobserve f name a = generateContext f name a 

{-# NOINLINE gdmobserve #-}
gdmobserve ::  (Observable a) => String -> a -> a
gdmobserve = gobserve observer

{- This gets called before observer, allowing us to mark
 - we are entering a, before we do case analysis on
 - our object.
 -}

{-# NOINLINE observer_ #-}
observer_ :: (a -> Parent -> a) -> a -> Parent -> a 
observer_ f a context = sendEnterPacket f a context

gdmobserver_ :: (GObservable f) => f a -> Parent -> f a
gdmobserver_ a context = gsendEnterPacket a context

{-# NOINLINE observer__ #-}
observer__ :: a -> Parent -> a
observer__ a context = sendNoEnterPacket a context

\end{code}

\begin{code}
data Parent = Parent
	{ observeParent :: !Int	-- my parent
	, observePort   :: !Int	-- my branch number
	} deriving Show
root = Parent 0 0
\end{code}


The functions that output the data. All are dirty.

\begin{code}
unsafeWithUniq :: (Int -> IO a) -> a
unsafeWithUniq fn 
  = unsafePerformIO $ do { node <- getUniq
		         ; fn node
		         }
\end{code}

\begin{code}
generateContext :: (a->Parent->a) -> String -> a -> a
generateContext f label orig = unsafeWithUniq $ \ node ->
     do { sendEvent node (Parent 0 0) (Observe label)
	; return (observer_ f orig (Parent
			{ observeParent = node
			, observePort   = 0
		        })
		  )
	}

send :: String -> ObserverM a -> Parent -> a
send consLabel fn context = unsafeWithUniq $ \ node ->
     do { let (r,portCount) = runMO fn node 0
	; sendEvent node context (Cons portCount consLabel)
	; return r
	}


sendEnterPacket :: (a -> Parent -> a) -> a -> Parent -> a
sendEnterPacket f r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context Enter
	; ourCatchAllIO (evaluate (f r context))
	                (handleExc context)
	}

gsendEnterPacket :: (GObservable f) => f a -> Parent -> f a
gsendEnterPacket r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context Enter
	; ourCatchAllIO (evaluate (gdmobserver r context))
	                (handleExc context)
	}

sendNoEnterPacket :: a -> Parent -> a
sendNoEnterPacket r context = unsafeWithUniq $ \ node ->
     do	{ sendEvent node context NoEnter
	; ourCatchAllIO (evaluate r)
	                (handleExc context)
	}

evaluate :: a -> IO a
evaluate a = a `seq` return a


sendObserveFnPacket :: CallStack -> ObserverM a -> Parent -> a
sendObserveFnPacket callStack fn context
  = unsafeWithUniq $ \ node ->
     do	{ let (r,_) = runMO fn node 0
	; sendEvent node context (Fun callStack)
	; return r
	}
\end{code}


%************************************************************************
%*									*
\subsection{Event stream}
%*									*
%************************************************************************

Trival output functions

\begin{code}
data Event = Event
		{ portId     :: !Int
		, parent     :: !Parent
		, change     :: !Change
		}
	deriving Show

data Change
	= Observe 	!String
	| Cons    !Int 	!String
	| Enter
        | NoEnter
	| Fun           !CallStack
	deriving Show

startEventStream :: IO ()
startEventStream = writeIORef events []

endEventStream :: IO [Event]
endEventStream =
	do { es <- readIORef events
	   ; writeIORef events badEvents 
	   ; return es
	   }

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent nodeId parent change =
	do { nodeId `seq` parent `seq` return ()
	   ; change `seq` return ()
	   ; takeMVar sendSem
	   ; es <- readIORef events
	   ; let event = Event nodeId parent change
	   ; writeIORef events (event `seq` (event : es))
	   ; putMVar sendSem ()
	   }

-- local
events :: IORef [Event]
events = unsafePerformIO $ newIORef badEvents

badEvents :: [Event]
badEvents = error "Bad Event Stream"

-- use as a trivial semiphore
{-# NOINLINE sendSem #-}
sendSem :: MVar ()
sendSem = unsafePerformIO $ newMVar ()
-- end local
\end{code}


%************************************************************************
%*									*
\subsection{unique name supply code}
%*									*
%************************************************************************

Use the single threaded version

\begin{code}
initUniq :: IO ()
initUniq = writeIORef uniq 1

getUniq :: IO Int
getUniq
    = do { takeMVar uniqSem
	 ; n <- readIORef uniq
	 ; writeIORef uniq $! (n + 1)
	 ; putMVar uniqSem ()
	 ; return n
	 }

peepUniq :: IO Int
peepUniq = readIORef uniq

-- locals
{-# NOINLINE uniq #-}
uniq :: IORef Int
uniq = unsafePerformIO $ newIORef 1

{-# NOINLINE uniqSem #-}
uniqSem :: MVar ()
uniqSem = unsafePerformIO $ newMVar ()
\end{code}



%************************************************************************
%*									*
\subsection{Global, initualizers, etc}
%*									*
%************************************************************************

\begin{code}
openObserveGlobal :: IO ()
openObserveGlobal =
     do { initUniq
	; startEventStream
	}

closeObserveGlobal :: IO [Event]
closeObserveGlobal =
     do { evs <- endEventStream
        ; putStrLn ""
	; return evs
	}
\end{code}


%************************************************************************
%*									*
\subsection{The CDS and converting functions}
%*									*
%************************************************************************

\begin{code}
type CallStack = [String]
emptyStack = [""]

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
\end{code}



%************************************************************************
%*									*
\subsection{A Pretty Printer}
%*									*
%************************************************************************

This pretty printer is based on Wadler's pretty printer.

\begin{code}
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
\end{code}

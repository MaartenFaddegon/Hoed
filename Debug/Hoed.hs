module Debug.Hoed
  ( observe
  , gdmobserve
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
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


import Debug.Hoed.Observe
import Debug.Hoed.Render

import Prelude hiding (Right)
import qualified Prelude
import System.IO
import Data.Maybe
import Control.Monad
import Data.Array as Array
import Data.List
import Data.Char
import System.Environment

import Language.Haskell.TH
import GHC.Generics

import Data.IORef
import System.IO.Unsafe

-- User interface imports:
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny (startGUI,defaultConfig,tpPort,tpStatic
                              , Window, UI, (#), (#+), (#.), string, on
                              )
import System.Process(system)

-- Library with our graph algorithms:
import Data.Graph.Libgraph

-- %************************************************************************
-- %*									*
-- \subsection{External start functions}
-- %*									*
-- %************************************************************************

-- Run the observe ridden code.

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

       ; let eqs   = ((sortBy byStack) . renderCompStmts) cdss2
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
-- Algorithmic Debugging

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
       mapM_ (onClick buttons img treeRef Right) 
             (zip (corButtons ts) (reverse ns))
       mapM_ (onClick buttons img treeRef Wrong)
             (zip (wrnButtons ts) (reverse ns))


--              Slice      Hr         CompStmt   Right    Wrong
type ElemSet = (UI.Element,UI.Element,UI.Element,UI.Element,UI.Element)

data OddEven = Odd | Even

divpack :: ElemSet -> OddEven -> UI UI.Element
divpack (e1,e2,e3,e4,e5) x
  = UI.div #. lbl x #+ map UI.element [e1,e2,e3,e4,e5]
    where lbl Odd  = "odd"
          lbl Even = "even"

onClick :: UI.Element -> UI.Element -> IORef CompGraph -> Judgement
        -> (UI.Element,Vertex) -> UI ()
onClick buttons img treeRef status (b,n) 
  = do on UI.click b $ \_ -> do 
        updateTree img treeRef (\tree -> markNode tree n status)
        -- UI.element b # UI.set UI.text "I have been clicked!"
        -- UI.element buttons # UI.set UI.children []
        

-- MF TODO: We may need to reconsider how Vertex is defined,
-- and how we determine equality. I think it could happen that
-- two vertices with equal equation but different stacks/relations
-- are now both changed.
markNode :: CompGraph -> Vertex -> Judgement -> CompGraph
markNode g v s = mapGraph (\v' -> if v' === v then v{status=s} else v') g
  where (===) :: Vertex -> Vertex -> Bool
        v1 === v2 = (equations v1) == (equations v2)

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

  where shw g = showWith g (showVertex $ faultyVertices g) showArc
        showVertex :: [Vertex] -> Vertex -> String
        showVertex fs v = showStatus fs v ++ ":\n" ++ showCompStmts v

        showStatus fs v
          | v `elem` fs = "Faulty"
          | otherwise   = (show . status) v

        showCompStmts = showCompStmts' . equations
        showCompStmts' [e] = show e
        showCompStmts' es  = foldl (\acc e-> acc ++ show e ++ ", ") "{" (init es) 
                             ++ show (last es) ++ "}"

        -- showVertex = show
        -- showVertex = (foldl (++) "") . (map show) . equations
        showArc _  = ""

faultyVertices :: CompGraph -> [Vertex]
faultyVertices = findFaulty_dag status




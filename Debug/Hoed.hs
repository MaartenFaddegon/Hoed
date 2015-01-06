module Debug.Hoed
  ( observe
  , gdmobserve
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
  , Observable(..) -- Class
  , runO	   -- IO a -> IO ()
  , logO
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

  , ccsToStrings -- TODO, we don't really wan't to export this, do we?
  ) where


import Debug.Hoed.Observe
import Debug.Hoed.Render
import Debug.Hoed.DemoGUI

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
import Data.Graph.Libgraph
import Graphics.UI.Threepenny


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
runO slices program = {- SCC "runO" -} do
  compGraph <- runO' program
  debugSession slices compGraph
  return ()

runO' :: IO a -> IO CompGraph
runO' program = {- SCC "runO" -} do
  args <- getArgs
  setPushMode (parseArgs args)
  hPutStrLn stderr "=== program output ===\n"
  cdss <- debugO program
  let cdss1 = rmEntrySet cdss
  let cdss2 = simplifyCDSSet cdss1
  let eqs   = ((sortBy byStack) . renderCompStmts) cdss2
  hPutStrLn stderr "\n=== CDS's before rendering === \n"
  hPutStrLn stderr (show cdss2)
  hPutStrLn stderr "\n=== Debug session === \n"
  hPutStrLn stderr (showWithStack eqs)
  return (mkGraph eqs)

logO :: FilePath -> IO a -> IO ()
logO filePath program = {- SCC "logO" -} do
  compGraph <- runO' program
  writeFile filePath (showGraph compGraph)
  return ()

  where showGraph g        = showWith g showVertex showArc
        showVertex Root    = "root"
        showVertex v       = showCompStmts v ++ "\nwith stack " 
                             ++ (show . equStack . head . equations $ v)
        showArc _          = ""
        showCompStmts      = showCompStmts' . equations
        showCompStmts' [e] = show e
        showCompStmts' es  = foldl (\acc e-> acc ++ show e ++ ", ") "{" (init es) 
                             ++ show (last es) ++ "}"
  

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
           } (demoGUI slices treeRef)

{-|
Module      : Debug.Hoed.Pure
Description : Lighweight algorithmic debugging based on observing intermediate values and the cost centre stack.
Copyright   : (c) 2000 Andy Gill, (c) 2010 University of Kansas, (c) 2013-2014 Maarten Faddegon
License     : BSD3
Maintainer  : hoed@maartenfaddegon.nl
Stability   : experimental
Portability : POSIX

Hoed is a tracer and debugger for the programming language Haskell. You can
trace a program by annotating functions in suspected modules and linking your
program against standard profiling libraries. 

After running the program a computation tree is constructed and displayed in a
web browser. You can freely browse this tree to get a better understanding of
your program. If your program misbehaves, you can judge the computation
statements in the tree as 'right' or 'wrong' according to your intention. When
enough statements are judged the debugger tells you the location of the fault
in your code.

<<http://www.cs.kent.ac.uk/people/rpg/mf357/hoedv2.0.0.png>>

I work on this debugger in the context of my Ph.D. research.
Read more about the theory behind Hoed at <http://maartenfaddegon.nl/#pub>.

Unlike Hoed.Stk, to debug your program with Hoed.Pure you can optimize your
program and do not need to enable profiling.

I am keen to hear about your experience with Hoed:
where did you find it useful and where would you like to see improvement?
You can send me an e-mail at hoed@maartenfaddegon.nl, or use the
github issue tracker <https://github.com/MaartenFaddegon/hoed/issues>.

-}

module Debug.Hoed.Pure
  ( -- * Basic annotations
    observe
  , traceOnly
  , runO
  , printO

    -- * Experimental annotations
  , observeTempl
  , observedTypes
  , observeCC
  , observe'
  , Identifier(..)
  ,(*>>=),(>>==),(>>=*)
  , logO

   -- * The Observable class
  , Observer(..)
  , Observable(..)
  , (<<)
  , thunk
  , nothunk
  , send
  , observeBase
  , observeOpaque
  , debugO
  , CDS(..)
  , Generic
  ) where


import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.DataDep
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.DemoGUI

import Prelude hiding (Right)
import qualified Prelude
import System.IO
import Data.Maybe
import Control.Monad
-- import Data.Array as Array
import Data.List
import Data.Ord
import Data.Char
import System.Environment

import Language.Haskell.TH
import GHC.Generics

import Data.IORef
import System.IO.Unsafe
import Data.Graph.Libgraph
import Graphics.UI.Threepenny(startGUI,defaultConfig,Config(..))

import System.Directory(createDirectoryIfMissing)


-- %************************************************************************
-- %*                                                                   *
-- \subsection{External start functions}
-- %*                                                                   *
-- %************************************************************************

-- Run the observe ridden code.

-- | run some code and return the Trace
debugO :: IO a -> IO Trace
debugO program = 
     do { initUniq
        ; startEventStream
        ; let errorMsg e = "[Escaping Exception in Code : " ++ show e ++ "]"
        ; ourCatchAllIO (do { program ; return () }) 
                        (hPutStrLn stderr . errorMsg)
        ; endEventStream
        }

-- | run some code and return the CDS structure (for when you want to write your own debugger).
debugO' :: IO a -> IO [CDS]
debugO' program = do
  events <- debugO program
  return (eventsToCDS events)

-- | Short for @runO . print@.
printO :: (Show a) => a -> IO ()
printO expr = runO (print expr)

-- | print a string, with debugging 
putStrO :: String -> IO ()
putStrO expr = runO (putStr expr)

-- | The main entry point; run some IO code, and debug inside it.
--   After the IO action is completed, an algorithmic debugging session is started at
--   @http://localhost:10000/@ to which you can connect with your webbrowser.
-- 
-- For example:
--
-- @ 
--   main = runO $ do print (triple 3)
--                    print (triple 2)
-- @
-- 

traceOnly :: IO a -> IO ()
traceOnly program = do
  debugO program
  return ()

runO :: IO a -> IO ()
runO program = do
  let slices = [] -- MF TODO: this whole slices business should probably just go?
  (compGraph,frt,ddt) <- runO' program
  debugSession slices compGraph ddt frt
  return ()

runO' :: IO a -> IO (CompTree,EventForest,ConstantTree)
runO' program = do
  hPutStrLn stderr "=== program output ===\n"
  events <- debugO program
  let cdss = eventsToCDS events
  let cdss1 = rmEntrySet cdss
  let cdss2 = simplifyCDSSet cdss1
  let eqs   = renderCompStmts cdss2

  let frt  = mkEventForest events
      rs   = filter isRootEvent events
      ds   = spans (reverse events)
      ct   = mkCompTree eqs ds
      ddt  = undefined

  -- hPutStrLn stderr "\n=== Events ===\n"
  -- hPutStrLn stderr $ unlines (map show events)
  -- writeFile ".HoedEvents" (unlines . map show . reverse $ events)

  -- hPutStrLn stderr "\n=== Result Dependencies ===\n"
  -- hPutStrLn stderr $ "found " ++ (show . length $ rs) ++ " root events"
  -- mapM_ (\r -> hPutStrLn stderr $ " - root event " ++ show (eventUID r) ++ " has " 
  --              ++ (show . length . dfsChildren frt $ r) ++ " child events and "
  --              ++ (show . length . topLevelApps frt $ r) ++ " toplevel app events"
  --              ++ ",\nevents: " ++ (unwords . map summarizeEvent . eventsInTree frt $ r) ) rs

  -- hPutStrLn stderr $ "EventForest has " ++ (show . length . concat . elems $ frt) ++ " children."
  -- hPutStrLn stderr $ "found " ++ (show . length $ cns) ++ " constants"
  -- hPutStrLn stderr (unlines . map show $ cns )
  -- hPutStrLn stderr (unlines . map show . arcs $ rdt)

  -- hPutStrLn stderr "\n=== Computation Statements ===\n"
  -- hPutStrLn stderr $ show eqs
  -- hPutStrLn stderr $ unlines . (map $ show . stmtUIDs) $ eqs

  hPutStrLn stderr "\n=== Statistics ===\n"
  let e  = length events
      n  = length eqs
      d  = treeDepth ct
      b  = fromIntegral (length . arcs $ ct ) / fromIntegral ((length . vertices $ ct) - (length . leafs $ ct))
  hPutStrLn stderr $ "e = " ++ show e
  hPutStrLn stderr $ "n = " ++ show n
  hPutStrLn stderr $ "d' = " ++ (show . length $ ds)
  hPutStrLn stderr $ "d = " ++ show d
  hPutStrLn stderr $ "b = " ++ show b

  -- hPutStrLn stderr "\n=== Debug Session ===\n"
  return (ct, frt, ddt)

  where summarizeEvent e = show (eventUID e) ++ ": " ++ summarizeChange (change e)
        summarizeChange (Observe l _ _ _) = show l
        summarizeChange (Cons _ c)        = show c
        summarizeChange c                 = show c
        showJ (Just s) = show s
        showJ Nothing  = "??"

logO :: FilePath -> IO a -> IO ()
logO filePath program = {- SCC "logO" -} do
  (compGraph,_,_) <- runO' program
  writeFile filePath (showGraph compGraph)
  return ()

  where showGraph g        = showWith g showVertex showArc
        showVertex RootVertex = ("root","")
        showVertex v       = (showCompStmt v, "")
        showArc _          = ""
        showCompStmt       = show . vertexStmt
  

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

debugSession :: [(String,String)] -> CompTree -> ConstantTree -> EventForest -> IO ()
debugSession slices tree ddt frt
  = do createDirectoryIfMissing True ".Hoed/wwwroot/css"
       treeRef <- newIORef tree
       startGUI defaultConfig
           { jsPort       = Just 10000
           , jsStatic     = Just "./.Hoed/wwwroot"
           } (guiMain slices treeRef ddt frt)

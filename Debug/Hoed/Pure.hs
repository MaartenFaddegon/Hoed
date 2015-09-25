{-|
Module      : Debug.Hoed.Pure
Description : Lighweight algorithmic debugging based on observing intermediate values and the cost centre stack.
Copyright   : (c) 2000 Andy Gill, (c) 2010 University of Kansas, (c) 2013-2015 Maarten Faddegon
License     : BSD3
Maintainer  : hoed@maartenfaddegon.nl
Stability   : experimental
Portability : POSIX

Hoed is a tracer and debugger for the programming language Haskell. You can
trace a program by annotating functions in suspected modules and linking your
program against standard profiling libraries.

To locate a defect with Hoed you annotate suspected functions and compile as usual. Then you run your program, information about the annotated functions is collected. Finally you connect to a debugging session using a webbrowser.

Let us consider the following program, a defective implementation of a parity function with a test property.

> import Test.QuickCheck
>
> isOdd :: Int -> Bool
> isOdd n = isEven (plusOne n)
>
> isEven :: Int -> Bool
> isEven n = mod2 n == 0
>
> plusOne :: Int -> Int
> plusOne n = n + 1
>
> mod2 :: Int -> Int
> mod2 n = div n 2
>
> prop_isOdd :: Int -> Bool
> prop_isOdd x = isOdd (2*x+1)
>
> main :: IO ()
> main = printO (prop_isOdd 1)
>
> main :: IO ()
> main = quickcheck prop_isOdd

Using the property-based test tool QuickCheck we find the counter example `1` for our property.

> ./MyProgram
> *** Failed! Falsifiable (after 1 test): 1

Hoed can help us determine which function is defective. We annotate the functions `isOdd`, `isEven`, `plusOne` and `mod2` as follows:

> import Debug.Hoed.Pure
>
> isOdd :: Int -> Bool
> isOdd = observe "isOdd" isOdd'
> isOdd' n = isEven (plusOne n)
>
> isEven :: Int -> Bool
> isEven = observe "isEven" isEven'
> isEven' n = mod2 n == 0
>
> plusOne :: Int -> Int
> plusOne = observe "plusOne" plusOne'
> plusOne' n = n + 1
>
> mod2 :: Int -> Int
> mod2 = observe "mod2" mod2'
> mod2' n = div n 2
>
> prop_isOdd :: Int -> Bool
> prop_isOdd x = isOdd (2*x+1)
>
> main :: IO ()
> main = printO (prop_isOdd 1)

After running the program a computation tree is constructed and displayed in a web browser.

> ./MyProgram
> False
> Listening on http://127.0.0.1:10000/

After running the program a computation tree is constructed and displayed in a
web browser. You can freely browse this tree to get a better understanding of
your program. If your program misbehaves, you can judge the computation
statements in the tree as 'right' or 'wrong' according to your intention. When
enough statements are judged the debugger tells you the location of the fault
in your code.

<<http://www.cs.kent.ac.uk/people/rpg/mf357/hoedv2.0.0.png>>

I work on this debugger in the context of my Ph.D. research.
Read more about the theory behind Hoed at <http://maartenfaddegon.nl/#pub>.

Hoed.Pure is recommended over Hoed.Stk because to debug your program with Hoed.Pure you can optimize your program and do not need to enable profiling.

I am keen to hear about your experience with Hoed: where did you find it useful and where would you like to see improvement? You can send me an e-mail at hoed@maartenfaddegon.nl, or use the github issue tracker <https://github.com/MaartenFaddegon/hoed/issues>.
-}

module Debug.Hoed.Pure
  ( -- * Basic annotations
    observe
  , runO
  , printO
  , logO

  -- * Property-based judging
  , runOwp
  , logOwp
  , Propositions(..)
  , PropType(..)
  , Proposition(..)
  , PropositionType(..)
  , Module(..)

    -- * Experimental annotations
  , traceOnly
  , observeTempl
  , observedTypes
  , observeCC

   -- * Parallel equality
  , ParEq(..)

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
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.DemoGUI
import Debug.Hoed.Pure.Prop
import Paths_Hoed(getDataDir)

import Prelude hiding (Right)
import qualified Prelude
import System.Process(system)
import System.IO
import Data.Maybe
import Control.Monad
import Data.List
import Data.Ord
import Data.Char
import System.Environment
import System.Directory(createDirectoryIfMissing)

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

runO :: IO a -> IO ()
runO program = do
  (trace,traceInfo,compTree,frt) <- runO' program
  debugSession trace traceInfo compTree frt
  return ()


-- | Use property based judging.

runOwp :: [Propositions] -> IO a -> IO ()
runOwp ps program = do
  (trace,traceInfo,compTree,frt) <- runO' program
  hPutStrLn stderr "\n=== Evaluating assigned properties ===\n"
  compTree' <- judge trace ps compTree
  debugSession trace traceInfo compTree' frt
  return ()

-- | Short for @runO . print@.
printO :: (Show a) => a -> IO ()
printO expr = runO (print expr)

-- | Only produces a trace. Useful for performance measurements.
traceOnly :: IO a -> IO ()
traceOnly program = do
  debugO program
  return ()


runO' :: IO a -> IO (Trace,TraceInfo,CompTree,EventForest)
runO' program = do
  createDirectoryIfMissing True ".Hoed/"
  putStrLn "=== program output ===\n"
  events <- debugO program
  let cdss = eventsToCDS events
  let cdss1 = rmEntrySet cdss
  let cdss2 = simplifyCDSSet cdss1
  let eqs   = renderCompStmts cdss2

  let frt  = mkEventForest events
      rs   = filter isRootEvent events
      ti   = traceInfo (reverse events)
      ds   = dependencies ti
      ct   = mkCompTree eqs ds

  -- hPutStrLn stderr "\n=== Events ===\n"
  -- hPutStrLn stderr $ unlines (map show . reverse $ events)
  writeFile ".Hoed/Events" (unlines . map show . reverse $ events)
  -- writeFile ".Hoed/CDSSet" (show cdss2)

  -- hPutStrLn stderr "\n=== Dependencies ===\n"
  -- hPutStrLn stderr $ unlines (map (\(m,n,msg) -> show m ++ " -> " ++ show n ++ " %" ++ msg) ds)

  -- hPutStrLn stderr "\n=== Computation Statements ===\n"
  -- hPutStrLn stderr $ show eqs
  -- hPutStrLn stderr $ unlines . (map $ show . stmtUIDs) $ eqs

  hPutStrLn stderr "\n=== Statistics ===\n"
  let e  = length events
      n  = length eqs
      d  = treeDepth ct
      b  = fromIntegral (length . arcs $ ct ) / fromIntegral ((length . vertices $ ct) - (length . leafs $ ct))
      m  = maximum . map (length . show) $ eqs
  hPutStrLn stderr $ "e = " ++ show e
  hPutStrLn stderr $ "n = " ++ show n
  hPutStrLn stderr $ "d' = " ++ (show . length $ ds)
  -- hPutStrLn stderr $ "d = " ++ show d
  hPutStrLn stderr $ "b = " ++ show b
  hPutStrLn stderr $ "m = " ++ show m

  -- hPutStrLn stderr "\n=== Debug Session ===\n"
  return (events, ti, ct, frt)

  where summarizeEvent e = show (eventUID e) ++ ": " ++ summarizeChange (change e)
        summarizeChange (Observe l _ _) = show l
        summarizeChange (Cons _ c)      = show c
        summarizeChange c               = show c
        showJ (Just s) = show s
        showJ Nothing  = "??"

-- | Trace and write computation tree to file. Useful for regression testing.
logO :: FilePath -> IO a -> IO ()
logO filePath program = {- SCC "logO" -} do
  (_,_,compTree,_) <- runO' program
  writeFile filePath (showGraph compTree)
  return ()

  where showGraph g        = showWith g showVertex showArc
        showVertex RootVertex = ("root","")
        showVertex v       = ("\"" ++ (escape . showCompStmt) v ++ "\"", "")
        showArc _          = ""
        showCompStmt       = take 25 . show . vertexStmt -- MF TODO: meer dan 25!

-- | As logO, but with property-based judging.
logOwp :: FilePath -> [Propositions] -> IO a -> IO ()
logOwp filePath properties program = do
  (trace,traceInfo,compTree,frt) <- runO' program
  hPutStrLn stderr "\n=== Evaluating assigned properties ===\n"
  compTree' <- judge trace properties compTree
  writeFile filePath (showGraph compTree')
  return ()

  where showGraph g        = showWith g showVertex showArc
        showVertex RootVertex = ("root","")
        showVertex v       = ("\"" ++ (escape . showCompStmt) v ++ "\"", "")
        showArc _          = ""
        showCompStmt s     = (show . vertexJmt) s ++ ": " ++ (show . vertexStmt) s

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

debugSession :: Trace -> TraceInfo -> CompTree -> EventForest -> IO ()
debugSession trace traceInfo tree frt
  = do createDirectoryIfMissing True ".Hoed/wwwroot/css"
       dataDir <- getDataDir
       system $ "cp " ++ dataDir ++ "/img/*png .Hoed/wwwroot/"
       treeRef <- newIORef tree
       startGUI defaultConfig
           { jsPort       = Just 10000
           , jsStatic     = Just "./.Hoed/wwwroot"
           } (guiMain trace traceInfo treeRef frt)

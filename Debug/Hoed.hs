{-|
Module      : Debug.Hoed
Description : Lighweight algorithmic debugging based on observing of intermediate values and the cost centre stack.
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

I made this debugger in the context of my Ph.D. research.
Read more about the theory behind Hoed at <http://maartenfaddegon.nl/#pub>.

I am keen to hear about your experience with Hoed:
where did you find it useful and where would you like to see improvement?
You can send me an e-mail at hoed@maartenfaddegon.nl, or use the
github issue tracker <https://github.com/MaartenFaddegon/hoed/issues>.

-}

module Debug.Hoed
  ( -- * Basic annotations
    observe
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

import System.Directory(createDirectoryIfMissing)


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

runO :: IO a -> IO ()
runO program = {- SCC "runO" -} do
  let slices = [] -- MF TODO: this whole slices business should probably just go?
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
        showVertex Root    = ("root","")
        showVertex v       = (showCompStmts v ++ "\nwith stack "
                              ++ (show . equStack . head . equations $ v), "")
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
  = do createDirectoryIfMissing True ".Hoed/wwwroot/css"
       writeFile ".Hoed/wwwroot/debug.css" stylesheet
       treeRef <- newIORef tree
       startGUI defaultConfig
           { tpPort       = Just 10000
           , tpStatic     = Just "./.Hoed/wwwroot"
           } (demoGUI slices treeRef)


stylesheet
  =  "div {\n"
  ++ "  padding:0;\n"
  ++ "  margin:0;\n"
  ++ "}\n"
  ++ ".buttons {\n"
  ++ "  float:top;\n"
  ++ "  height:50vh;\n"
  ++ "  overflow-y: scroll;\n"
  ++ "  overflow-x: hidden;\n"
  ++ "}\n"
  ++ ".nowrap {\n"
  ++ "  white-space: nowrap;\n"
  ++ "}\n"
  ++ ".odd {\n"
  ++ "  background-color: lightgray;\n"
  ++ "  padding: 10px 0;\n"
  ++ "}\n"
  ++ ".even {\n"
  ++ "  background-color: white;\n"
  ++ "  padding: 10px 0;\n"
  ++ "}\n"

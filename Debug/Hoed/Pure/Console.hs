module Debug.Hoed.Pure.Console(debugSession) where
import Debug.Hoed.Pure.ReadLine
import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.Prop
import Debug.Hoed.Pure.Serialize

debugSession :: Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
debugSession trace traceInfo tree frt ps
  = do noBuffering
       mainLoop

help :: IO ()
help = putStr
  $  "help              Print this help message.\n"
  ++ "observe [regexp]  Print computation statements that matching a regular\n"
  ++ "                  expression. The empty expression matches all statements.\n"
  ++ "adb               Start algorithmic debugging.\n"
  ++ "exit              leave debugging session\n"

mainLoop :: IO ()
mainLoop = do
  i <- readLine "hdb> " ["adb", "observe", "help"]
  case words i of
    ["adb", n]          -> adb n
    ["observe", regexp] -> do printStmts regexp; mainLoop
    ["observe"]         -> do printStmts ""; mainLoop
    _                   -> do help; mainLoop

printStmts :: String -> IO ()
printStmts regexp = do
  putStrLn "TODO print statements here"

adb :: String -> IO ()
adb n = do
  putStr "<statement> ? "
  i <- getLine
  case i of
    "exit" -> mainLoop
    _      -> adb n

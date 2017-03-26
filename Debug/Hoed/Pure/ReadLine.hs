module Debug.Hoed.Pure.ReadLine where
import System.IO
import Data.List

noBuffering :: IO ()
noBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

readLine :: String -> [String] -> IO String
readLine ps completions = do
  putStr ps
  loop ""
  where
  loop curLine = do 
    c <- getChar
    case c of
      '\n'   -> return curLine
      '\DEL' -> do putStr "\b\b\b   \b\b\b"
                   loop (case curLine of [] -> []; _ -> init curLine)
      '\t'   -> case filter (isPrefixOf curLine) completions of
                  [cmd] -> do
                    putStr $ drop (length curLine) cmd
                    loop cmd
                  completions' -> do
                    putStr $ "\b\n" ++ unlines completions' ++ ps ++ curLine
                    loop curLine
      _      -> loop $ curLine ++ [c]

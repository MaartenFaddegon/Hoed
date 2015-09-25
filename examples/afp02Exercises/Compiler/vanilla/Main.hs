module Main where

import Syntax
import Parser
import Interpreter
import Machine
import Compiler

main = do
  source <- getContents
  let prog = parse source
  putStrLn "interpreted:"
  print (obey prog)
  putStrLn "compiled:"
  print (exec (compile prog))

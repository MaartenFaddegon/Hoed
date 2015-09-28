module Main where

import Debug.Hoed.Pure
import Syntax
import Parser
import Interpreter
import Machine
import Compiler

main = runO $ do
  let prog = parse gcdSource
  putStrLn "interpreted:"
  print (obey prog)
  putStrLn "compiled:"
  print (exec (compile prog))

gcdSource :: String
gcdSource = "x := 148; y := 58;\nwhile ~(x=y) do\n  if x < y then y := y - x\n  else x := x - y\n  fi\nod;\nprint x\n"

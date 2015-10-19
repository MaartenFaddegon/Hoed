-- A program with unexpected output.

import MyModule
import Debug.Hoed.Pure

main = testO prop_idemSimplify (Mul (Const 1) (Const 2))

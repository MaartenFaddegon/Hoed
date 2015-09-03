-- A program with unexpected output.

import MyModule
import Debug.Hoed.Pure

-- main = quickcheck prop_idemSimplify
main = logOwp "hoed-tests-Prop-t0.graph" properties $ print $ prop_idemSimplify (Mul (Const 1) (Const 2))
  where
  properties = [ Property "one"      "MyModule" "prop_idemOne"      "../Prop/t0/"
               , Property "zero"     "MyModule" "prop_idemZero"     "../Prop/t0/"
               , Property "simplify" "MyModule" "prop_idemSimplify" "../Prop/t0/"
               ]

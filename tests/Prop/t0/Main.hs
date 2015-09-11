-- A program with unexpected output.

import MyModule
import Debug.Hoed.Pure

-- main = quickcheck prop_idemSimplify
main = logOwp "hoed-tests-Prop-t0.graph" properties $ print $ prop_idemSimplify (Mul (Const 1) (Const 2))
  where
  properties = [ Propositions [("prop_idemOne",myModule)]      PropertiesOf "one"
               , Propositions [("prop_idemZero",myModule)]     PropertiesOf "zero"
               , Propositions [("prop_idemSimplify",myModule)] PropertiesOf "simplify"
               ]
  myModule = Module "MyModule" "../Prop/t0/"

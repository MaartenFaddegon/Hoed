-- A program with unexpected output.

import MyModule
import Debug.Hoed.Pure

-- main = quickcheck prop_idemSimplify
main = logOwp "hoed-tests-Prop-t0.graph" properties $ print $ prop_idemSimplify (Mul (Const 1) (Const 2))
  where
  properties = [ Propositions [(BoolProposition,myModule,"prop_idemOne",[1])]      PropertiesOf "one" []
               , Propositions [(BoolProposition,myModule,"prop_idemZero",[1])]     PropertiesOf "zero" []
               , Propositions [(BoolProposition,myModule,"prop_idemSimplify",[1])] PropertiesOf "simplify" []
               ]
  myModule = Module "MyModule" "../Prop/t0/"

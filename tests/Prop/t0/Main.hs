-- A program with unexpected output.

import MyModule
import Debug.Hoed.Pure

-- main = quickcheck prop_idemSimplify
main = logOwp Bottom "hoed-tests-Prop-t0.graph" properties $ print $ prop_idemSimplify (Mul (Const 1) (Const 2))
  where
  properties = [ Propositions [(BoolProposition,myModule,"prop_idemOne",[Argument 0])]      PropertiesOf "one" []
               , Propositions [(BoolProposition,myModule,"prop_idemZero",[Argument 0])]     PropertiesOf "zero" []
               , Propositions [(BoolProposition,myModule,"prop_idemSimplify",[Argument 0])] PropertiesOf "simplify" []
               ]
  myModule = Module "MyModule" "../Prop/t0/"

-- A program with unexpected output.

import MyModule
import Debug.Hoed.Pure

-- main = quickcheck prop_idemSimplify
main = logOwp Bottom "hoed-tests-Prop-t0.graph" properties $ print $ prop_idemSimplify (Mul (Const 1) (Const 2))
  where
  properties = [ Propositions [mkProposition myModule "prop_idemOne"
                                `ofType` BoolProposition
                                `withSignature` [Argument 0]] PropertiesOf "one" []
               , Propositions [mkProposition myModule "prop_idemZero"
                                `ofType` BoolProposition
                                `withSignature` [Argument 0]] PropertiesOf "zero" []
               , Propositions [mkProposition myModule "prop_idemSimplify"
                                `ofType` BoolProposition
                                `withSignature` [Argument 0]] PropertiesOf "simplify" []
               ]
  myModule = Module "MyModule" "../Prop/t0/"

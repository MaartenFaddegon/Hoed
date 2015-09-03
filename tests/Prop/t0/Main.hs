import MyModule
import Debug.Hoed.Pure

--------------------------------------------------------------------------------
-- A program with unexpected output
main = logOwp "hoed-tests-Prop-t0.graph" $ print $ prop_idemSimplify (Mul (Const 1) (Const 2))

module Main where

import Behaviour
import Interpreter
import Compiler
import Machine
import Syntax
import Value

import Monad
import QuickCheck2

-- test data generators

arbName :: Gen Name
arbName =
  sized $ \n -> elements [ [c] | c <- take (n `div` 2 + 1) ['a'..'z'] ]

instance Arbitrary Value where
  arbitrary =
    frequency [ (4, Num `liftM` arbitrary)
              , (4, Log `liftM` arbitrary)
              , (1, return Wrong)
              ]

instance Arbitrary Expr where
  arbitrary = sized arbExpr
   where
    arbExpr n =
      frequency $
        [ (1, liftM Var arbName)
        , (1, liftM Val arbitrary)
        ] ++
        concat
        [ [ (2, liftM2 Uno arbitrary arbExpr')
          , (4, liftM3 Duo arbitrary arbExpr2 arbExpr2)
          ]
        | n > 0
        ]
     where
      arbExpr' = arbExpr (n-1)
      arbExpr2 = arbExpr (n `div` 2)

  shrink (Uno op e)     = [ e ]
                       ++ [ Uno op e' | e' <- shrink e ]
  shrink (Duo op e1 e2) = [ e1, e2 ]
                       ++ [ Duo op e1' e2 | e1' <- shrink e1 ]
                       ++ [ Duo op e1 e2' | e2' <- shrink e2 ]
  shrink _              = []

instance Arbitrary Op1 where
  arbitrary = elements [ Not, Minus ]
  
instance Arbitrary Command where
  arbitrary = sized arbCommand
   where
    arbCommand n =
      frequency $
        [ (1, return Skip)
        , (4, liftM2 (:=) arbName arbitrary)
        , (3, liftM  Print arbitrary)
        ] ++
        concat
        [ [ (4, liftM2 (:->) arbCommand2 arbCommand2)
          , (4, liftM3 If    arbitrary arbCommand2 arbCommand2)
          , (4, liftM2 While arbitrary arbCommand')
          ]
        | n > 0
        ]
     where
      arbCommand' = arbCommand (n-1)
      arbCommand2 = arbCommand (n `div` 2)

  shrink Skip         = []
  shrink (x := e)     = [ Skip ]
                     ++ [ x := e' | e' <- shrink e ]
  shrink (Print e)    = [ Skip ]
                     ++ [ Print e' | e' <- shrink e ]
  shrink (c1 :-> c2)  = [ c1, c2 ]
                     ++ [ c1' :-> c2  | c1' <- shrink c1 ]
                     ++ [ c1  :-> c2' | c2' <- shrink c2 ]
  shrink (If e c1 c2) = [ c1, c2 ]
                     ++ [ If e' c1 c2 | e'  <- shrink e ]
                     ++ [ If e c1' c2 | c1' <- shrink c1 ]
                     ++ [ If e c1 c2' | c2' <- shrink c2 ]
  shrink (While e c)  = [ c ]
                     ++ [ While e' c | e' <- shrink e ]
                     ++ [ While e c' | c' <- shrink c ]

instance Arbitrary Op2 where
  arbitrary =  
    elements [ And, Or, Mul, Add, Mod, Eq, Sub, Div, Mod, Less, LessEq ]

-- helper functions

(=~=) :: Eq a => Trace a -> Trace a -> Property
s =~= t = forAll arbitrary $ \n -> n > 0 ==> approx n s t

-- properties

arbitraryClosedExpr :: Gen Expr
arbitraryClosedExpr = sized arbExpr
 where
  arbExpr n =
    frequency $
      [ (1, liftM Val arbitrary)
      ] ++
      concat
      [ [ (2, liftM2 Uno arbitrary arbExpr')
        , (2, liftM3 Duo arbitrary arbExpr2 arbExpr2)
        ]
      | n > 0
      ]
   where
    arbExpr' = arbExpr (n-1)
    arbExpr2 = arbExpr (n `div` 2)

prop_CongruenceExpr :: Property
prop_CongruenceExpr =
  forAllShrink arbitraryClosedExpr shrink $ \e ->
    let p  = Print e
        t1 = obey p
        t2 = exec (compile p)
     in (t1 /= Wrong :> End || t2 /= Wrong :> End) ==>
          t1 =~= t2

prop_Congruence :: Command -> Property
prop_Congruence p =
  let t1 = obey p
      t2 = exec (compile p)
   in (t1 /= Crash || t2 /= Crash) ==>
        t1 =~= t2

-- main

main :: IO ()
main =
  do putStrLn "-- Expressions:"
     quickCheck prop_CongruenceExpr
     putStrLn "-- Programs:"
     quickCheck prop_Congruence

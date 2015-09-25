module Main where

import Behaviour
import Interpreter
import Compiler
import Machine
import Syntax
import Value

import Monad
import QuickCheck2

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

instance Arbitrary Op2 where
  arbitrary =  
    elements [ And, Or, Mul, Add, Mod, Eq, Sub, Div, Mod, Less, LessEq ]

noVar :: Expr -> Bool
noVar (Var x)      = False
noVar (Val v)      = True
noVar (Uno op a)   = noVar a
noVar (Duo op a b) = noVar a && noVar b

arbClosedExpr :: Gen Expr
arbClosedExpr = sized arbExpr
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

traceApprox n trace1 trace2 =
  (trace1 /= Crash || trace2 /= Crash) ==>
      approx n trace1 trace2

prop_CongruenceExpr =
  forAll arbClosedExpr $ \e ->
    let p         = Print e
        traceObey = obey p
        traceExec = exec (compile p)
     in (traceObey /= Crash || traceExec /= Crash) ==>
          (traceObey /= (Wrong :> End) || traceExec /= (Wrong :> End)) ==>
            traceObey == traceExec

prop_Congruence p n =
  n > 0 ==>
    let traceObey = obey p
        traceExec = exec (compile p)
     in (traceObey /= Crash || traceExec /= Crash) ==>
          approx n traceObey traceExec

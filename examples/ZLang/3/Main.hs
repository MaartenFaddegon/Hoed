import Parser as P
import TypeInfer as T
import MatchCheck as MC
import Control.Monad.Trans.Writer.Lazy
import qualified Data.List as List

-- TEST RELATED BEGIN
import Types
import TypedAst
import qualified Data.Map as Map
import Debug.Hoed.Pure

failType = Record False [("a",IntType),("b",StringType)]

m1 = (TRecordMatchExpr [("a",(TIntMatchExpr 42,IntType)),("b",(TStringMatchExpr "abc",StringType))],Record False [("a",IntType),("b",StringType)])
m2 = (TRecordMatchExpr [("a",(TVarMatch "n",IntType)),("b",(TStringMatchExpr "def",StringType))],Record False [("a",IntType),("b",StringType)])
m3 = (TRecordMatchExpr [("a",(TVarMatch "n",IntType)),("b",(TVarMatch "s",StringType))],Record False [("a",IntType),("b",StringType)])
 
testCovering ty matches = covering Map.empty (ideal Map.empty ty) matches == [Covered]
-- TEST RELATED END

concreteTest = testCovering failType [m1, m2, m3]

main = printO $ concreteTest

{-
main :: IO ()
main = let path = "../test.z"
       in do content <- readFile path
             case P.parse content of
               Left err -> print err
               Right ast -> do
                 (typed, env, subst) <- T.infer ast
                 let (b, badMatches) = runWriter (MC.matchCheck env typed)
                 mapM putStrLn (List.map MC.formatMatchWarning badMatches)
                 case b of
                  True -> return () --Continue compilation
                  False -> return () --Abort compilation
-}

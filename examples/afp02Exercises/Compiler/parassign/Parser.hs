module Parser (parse) where

import Char
import Syntax
import Value

--------------------------
-- Part 1: generic parsing

type Parser a = String -> [(a,String)]

succeed :: a -> Parser a
succeed v inp = [(v,inp)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p []	 = []
satisfy p (x:xs) = if p x then [(x,xs)] else []

lit :: Char -> Parser Char
lit x = satisfy (==x)

infixl 9 ..., *.., ..*
infix  8 `using`
infixr 7 |||

(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 inp	 = p1 inp ++ p2 inp

(...) :: Parser a -> Parser b -> Parser (a,b)
(...) p1 p2 inp	= concatMap f1 (p1 inp)
                  where
		  f1 (v1,inp1) =  map f2 (p2 inp1)
		                  where
                                  f2 (v2,inp2) = ((v1,v2),inp2)

(..*) :: Parser a -> Parser b -> Parser a
p1 ..* p2 = (p1 ... p2) `using` fst

(*..) :: Parser a -> Parser b -> Parser b
p1 *.. p2 = (p1 ... p2) `using` snd

infix `opt`

opt :: Parser a -> a -> Parser a
opt p v inp = [head ((p ||| succeed v) inp)]

using :: Parser a -> (a->b) -> Parser b
using p f inp = map (\(v,out) -> (f v,out)) (p inp)

many, some :: Parser a -> Parser [a]
many p = ((p ... many p) `using` cons) `opt` []
some p = (p ... many p) `using` cons

cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs

the :: [(a,String)] -> a
the ((x,""):_) = x
the (_:rest)   = the rest

-------------------------
-- Part 2: parser for Imp

command ::Parser Command
command = nonSeqCommand ...
          many (lit ';' *.. white *.. nonSeqCommand)
          `using` foldr1 (:->) . cons

nonSeqCommand :: Parser Command
nonSeqCommand =
  key "skip" `using` const Skip |||
  key "print" *.. expr `using` Print |||
  key "if" *.. expr ..*
    key "then" ... command ..*
    key "else" ... command ..*
    key "fi" `using` uncurry (uncurry If) |||
  key "while" *.. expr ..*
    key "do" ... command ..*
    key "od" `using` uncurry While |||
  listOf name ..* key ":=" ... listOf expr `using` uncurry (:=)

key :: String -> Parser ()
key k = foldr1 (*..) (map lit k) *.. white *.. succeed ()

expr :: Parser Expr
expr = term ...
       many (op2 ... term)
       `using` uncurry duoChain

duoChain :: Expr -> [(Op2,Expr)] -> Expr
duoChain e [] = e
duoChain e ((o,e'):oes) = Duo o e (duoChain e' oes)

term :: Parser Expr
term =
  name `using` Var |||
  value `using` Val |||
  op1 ... term `using` uncurry Uno |||
  key "(" *.. expr ..* key ")"

op1 :: Parser Op1
op1 = key "~" *.. succeed Not |||
      key "-" *.. succeed Minus

op2 :: Parser Op2
op2 = key "&"  *.. succeed And    |||
      key "|"  *.. succeed Or     |||
      key "+"  *.. succeed Add    |||
      key "-"  *.. succeed Sub    |||
      key "*"  *.. succeed Mul    |||
      key "/"  *.. succeed Div    |||
      key "\\" *.. succeed Mod    |||
      key "<"  *.. succeed Less   |||
      key "<=" *.. succeed LessEq |||
      key "="  *.. succeed Eq

name :: Parser Name
name = some (satisfy isLower) ..* white

value :: Parser Value
value = key "T" *.. succeed (Log True)  |||
        key "F" *.. succeed (Log False) |||
        some(satisfy isDigit) ..* white `using` Num . read

white :: Parser String
white = many (satisfy isSpace)

listOf :: Parser a -> Parser [a]
listOf p = p ... many (key "," *.. p) `using` cons 

parse :: String -> Command
parse s = the (command s)


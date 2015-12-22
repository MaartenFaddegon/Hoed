module Ast where
import Types

data Decl = TypeDecl String [String] Type
          | FunDecl String [String] [MatchExpr] (Maybe Type) Statement
    deriving (Show)

data Expr = IntExpr Int
          | RealExpr Double
          | BoolExpr Bool
          | StringExpr String
          | OrExpr Expr Expr
          | AndExpr Expr Expr
          | EqExpr Expr Expr
          | NeqExpr Expr Expr
          | LtExpr Expr Expr
          | GtExpr Expr Expr
          | LeExpr Expr Expr
          | GeExpr Expr Expr
          | AddExpr Expr Expr
          | SubExpr Expr Expr
          | MultExpr Expr Expr
          | DivExpr Expr Expr
          | UnaryMinusExpr Expr
          | BangExpr Expr
          | CallExpr Expr Expr
          | TypeConstrainedExpr Expr Type
          | ListExpr [Expr]
          | TupleExpr [Expr]
          | RecordExpr [(String, Expr)]
          | LValue LValueExpr
          | LambdaExpr [MatchExpr] Statement
  deriving (Show)

data MatchExpr = TupleMatchExpr [MatchExpr]
               | ListMatchExpr [MatchExpr]
               | RecordMatchExpr [(String, MatchExpr)]
               | TypedMatchExpr MatchExpr Type
               | VarMatch String
               | IntMatchExpr Int
               | StringMatchExpr String
               | BoolMatchExpr Bool
  deriving (Show)

data LValueExpr = VarExpr String
                | FieldAccessExpr LValueExpr String
                | ArrayAccessExpr LValueExpr Expr
  deriving (Show)

data Statement = IfStatement Expr Statement (Maybe Statement)
               | WhileStatement Expr Statement
               | ForStatement MatchExpr Expr Statement
               | CompoundStatement [Statement]
               | AssignStatement (Either MatchExpr LValueExpr) Expr
               | MatchStatement Expr [(MatchExpr, Statement)]
               | ReturnStatement Expr
               | BreakStatement
               | ContinueStatement
               | DeclStatement Decl
  deriving (Show)

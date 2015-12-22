module IR where
import Types

data Expr
  = BinOp BOp TExpr TExpr
  | Mem TExpr
  | Temp Int
  | Eseq Stmt TExpr
  | Name String
  | IntConst Int
  | RealConst Double
  | Call TExpr [TExpr]

newtype TExpr = TExpr Expr Type

data BOp
  = Plus | Minus | Mult | Div |
    Beq | Bneq | Blt | Bgt | Ble | Bge
    
data ROp
  = Req | Rneq | Rlt | Rgt | Rle | Rge
    
data Stmt
  = Label String
  | CJump ROp TExpr TExpr String String
  | Move TExpr TExpr
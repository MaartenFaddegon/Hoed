module Parser where
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Applicative hiding (many, (<|>))
import Control.Monad.State
import Data.Char
import Data.Maybe
import Types
import Ast

type IParser a = ParsecT String () (State SourcePos) a

parse :: SourceName -> Either ParseError [Decl]
parse input = runIndent "" $ runParserT (many1 (decl <* spaces) <* eof) () "" input

spaces' = many (char ' ')

-- Declarations
typeDecl :: IParser Decl
typeDecl = do
    string "type"
    spaces
    name <- id_
    typevarsM <- optionMaybe $ char '<' >> spaces' >>
                   sepBy1 id_ (spaces' >> char ',' >> spaces') <*
                     spaces' <* char '>'
    spaces
    char '='
    spaces
    t <- type_
    spaces
    return (TypeDecl name (fromMaybe [] typevarsM) t)

funDecl :: IParser Decl
funDecl = withPos $ do
    string "fun"
    spaces'
    name <- id_
    spaces'
    typevarsM <- optionMaybe $ char '<' >> spaces' >>
                   sepBy1 id_ (spaces' >> char ',' >> spaces') <*
                     spaces' <* char '>'
    spaces'
    args <- sepBy1 matchExpr spaces'
    spaces'
    typeM <- optionMaybe $ string "->" >> spaces' >> type_
    spaces'
    char '='
    spaces
    sameOrIndented
    s <- block statement
    spaces
    fundecls <- many (funDeclWithName name)
    return $ FunDecl name (fromMaybe [] typevarsM) args typeM (makeCompound s)

funDeclWithName :: String -> IParser (String, [String], [MatchExpr], Maybe Type, Statement)
funDeclWithName name = do
    string "fun"
    spaces'
    string name
    spaces'
    typevarsM <- optionMaybe $ char '<' >> spaces' >>
                   sepBy1 id_ (spaces' >> char ',' >> spaces') <*
                     spaces' <* char '>'
    spaces'
    args <- sepBy1 matchExpr spaces'
    spaces'
    typeM <- optionMaybe $ string "->" >> spaces' >> type_
    spaces'
    char '='
    spaces
    sameOrIndented
    s <- block statement
    spaces
    return (name, fromMaybe [] typevarsM, args, typeM, makeCompound s)
    
decl = funDecl
   <|> typeDecl

-- Types
type_ :: IParser Type
type_ = chainl1 arrowType unionType

unionType :: IParser (Type -> Type -> Type)
unionType = do
    char '|'
    spaces
    return union

keywords = ["type", "fun", "fn", "if", "else",
            "while", "for", "in", "match",
            "return", "break", "continue",
            "or", "and", "True", "False"]

id_ :: IParser String
id_ = do
    c <- letter <|> char '_'
    s <- many (alphaNum <|> oneOf "_'")
    if elem (c:s) keywords then
      fail ""
    else
      spaces'
    return $ c:s

arrayType :: IParser Type
arrayType = do
    char '['
    spaces
    t <- type_
    spaces
    char ']'
    spaces
    return $ Array t

tupleType :: IParser Type
tupleType = do
    char '('
    spaces
    ts <- sepBy type_ (spaces >> char ',' >> spaces)
    spaces
    char ')'
    spaces
    return $ Tuple ts

recField :: IParser (String, Type)
recField = do
    name <- id_
    spaces
    char ':'
    spaces
    t <- type_
    spaces
    return (name, t)

recordType :: IParser Type
recordType = do
    char '{'
    spaces
    ts <- sepBy recField (spaces >> char ',' >> spaces)
    spaces
    char '}'
    spaces
    return $ Record False ts

nameType :: IParser Type
nameType = do
    name <- id_
    let t = if name == "Int" then Just IntType
            else if name == "Bool" then Just BoolType
            else if name == "Real" then Just RealType
            else if name == "String" then Just StringType
            else Nothing
    tys <- option [] (char '<' >> sepBy1 type_ (spaces' >> char ',' >> spaces') <* char '>')
    spaces'
    return $ fromMaybe (Name name tys) t

arrowType :: IParser Type
arrowType = chainr1 simpleType arrow

arrow :: IParser (Type -> Type -> Type)
arrow = do
    string "->"
    spaces
    return Arrow

simpleType :: IParser Type
simpleType = arrayType
         <|> tupleType
         <|> recordType
         <|> nameType

-- Statements
statement :: IParser Statement
statement = try ifStatement
        <|> try whileStatement
        <|> try forStatement
        <|> try matchStatement
        <|> try returnStatement
        <|> try breakStatement
        <|> try continueStatement
        <|> try declStatement
        <|> assignStatement

makeCompound [s] = s
makeCompound s = CompoundStatement s

ifStatement = withPos $ do
    e <- ifHeader
    indented
    spaces
    s <- block statement
    s' <- else_
    spaces
    return $ makeIf e s s'
  where
    makeIf e sThen sElseM =
      IfStatement e (makeCompound sThen) (liftM makeCompound sElseM)

    ifHeader = do
        string "if"
        spaces'
        e <- expr
        spaces'
        char ':'
        spaces' -- TODO: Should be spaces instead?
        return e

    elseHeader b = do
        string "else"
        spaces'
        char ':'
        spaces
        return (Just b)

    else_ = do
        -- We use Maybe Bool to determine what kind of situation we're in:
        -- If the correct amount of indentation is present, followed by 'else',
        --   then we parse the statements following this.
        -- If there is an else statement that's indented further than the current indentation
        --   then report an error.
        -- Otherwise we do nothing.
        x <- lookAhead ((checkIndent >> (elseHeader True))
                    <|> (indented >> (elseHeader False))
                    <|> return Nothing)
        case x of
          Just True ->
            elseHeader () >>
            indented >>
            optionMaybe (block statement)
          Just False -> fail ""
          Nothing -> return Nothing

whileStatement = withPos $ do
    e <- whileHeader
    indented
    s <- block statement
    spaces
    return $ WhileStatement e (makeCompound s)
  where whileHeader = do
            string "while"
            spaces'
            e <- expr
            spaces'
            char ':'
            spaces
            return e

forStatement = withPos $ do
    (me, e) <- forHeader
    indented
    s <- block statement
    spaces
    return $ ForStatement me e (makeCompound s)
  where forHeader = do
            string "for"
            spaces'
            me <- matchExpr
            spaces'
            string "in"
            spaces'
            e <- expr
            spaces'
            char ':'
            spaces
            return (me, e)

assignStatement = try assignMatchExpr
              <|> assignLValueExpr
  where assignMatchExpr = do
            me <- matchExpr
            spaces'
            char '='
            spaces
            sameOrIndented
            e <- expr
            spaces
            return $ AssignStatement (Left me) e
        assignLValueExpr = do
            lve <- lvalueExpr
            spaces'
            char '='
            spaces
            sameOrIndented
            e <- expr
            spaces
            return $ AssignStatement (Right lve) e

matchStatement = withPos $ do
    string "match"
    spaces'
    e <- expr
    spaces'
    char ':'
    spaces
    indented
    actions <- block matchAction
    spaces
    return $ MatchStatement e actions
  where matchAction = do
            me <- matchExpr
            spaces'
            string "=>"
            spaces
            indented
            s <- block statement
            spaces
            return (me, makeCompound s)

returnStatement = do
    string "return"
    spaces'
    e <- expr
    spaces
    return $ ReturnStatement e

breakStatement = do
    string "break"
    spaces
    return BreakStatement

continueStatement = do
    string "continue"
    spaces
    return ContinueStatement

declStatement = do
    d <- decl
    return $ DeclStatement d

-- Expressions
expr = orExpr

orExpr :: IParser Expr
orExpr = chainl1 andExpr (op "or" OrExpr)

andExpr :: IParser Expr
andExpr = chainl1 eqExpr (op "and" AndExpr)

eq = op "==" EqExpr
 <|> op "!=" NeqExpr

eqExpr :: IParser Expr
eqExpr = chainl1 relExpr eq

rel = try (op "<=" LeExpr)
  <|> try (op ">=" GeExpr)
  <|> op "<" LtExpr
  <|> op ">" GtExpr

relExpr :: IParser Expr
relExpr = chainl1 addExpr rel

plusminus :: IParser (Expr -> Expr -> Expr)
plusminus = op "+" AddExpr
        <|> op "-" SubExpr

addExpr :: IParser Expr
addExpr = chainl1 multExpr plusminus

timesdiv :: IParser (Expr -> Expr -> Expr)
timesdiv = op "*" MultExpr
        <|> op "/" DivExpr

op :: String -> (Expr -> Expr -> Expr) -> IParser (Expr -> Expr -> Expr)
op s f = do
    string s
    spaces'
    return f

multExpr :: IParser Expr
multExpr = chainl1 unaryExpr timesdiv

unaryExpr :: IParser Expr
unaryExpr = bangExpr
      <|> unaryPlusExpr
      <|> unaryMinusExpr

unaryPlusExpr :: IParser Expr
unaryPlusExpr = do
    char '+'
    spaces'
    e <- unaryExpr
    return e

unaryMinusExpr :: IParser Expr
unaryMinusExpr = do
    char '-'
    spaces'
    e <- unaryExpr
    return $ UnaryMinusExpr e

bangExpr :: IParser Expr
bangExpr = parseBangExpr
       <|> callExpr
  where
    parseBangExpr = do
      char '!'
      spaces'
      e <- unaryExpr
      return $ BangExpr e

callExpr :: IParser Expr
callExpr = chainl1 primaryExpr (return CallExpr)

primaryExpr :: IParser Expr
primaryExpr = do
    e <- primaryExpr'
    spaces'
    t <- maybeType
    spaces'
    return $ maybe e (TypeConstrainedExpr e) t
  where
    primaryExpr' = literalExpr
               <|> listExpr
               <|> tupleExpr
               <|> recordExpr
               <|> try lambdaExpr
               <|> try ((return LValue) <*> lvalueExpr)

maybeType = optionMaybe $ try $ do
                char ':'
                spaces'
                t <- type_
                spaces'
                return t

literalExpr :: IParser Expr
literalExpr = try ((return RealExpr) <*> real)
          <|> (return IntExpr) <*> integer
          <|> (return BoolExpr) <*> bool
          <|> (return StringExpr) <*> string_

integer :: IParser Int
integer = do
    n <- rd <$> (minus <|> number)
    spaces'
    return n
  where rd     = read :: String -> Int
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

real :: IParser Double
real = do
    n <- fmap read $ (++) <$> number <*> decimal
    spaces'
    return n
  where rd = read :: String -> Float
        decimal = (:) <$> char '.' <*> number
        number = many1 digit

bool :: IParser Bool
bool = trueExpr
   <|> falseExpr
  where trueExpr = do
          string "True"
          spaces'
          return True
        falseExpr = do
          string "False"
          spaces'
          return False

string_ :: IParser String
string_ = do
    char '"'
    s <- many (noneOf "\"")
    char '"'
    spaces'
    return s

listExpr :: IParser Expr
listExpr = do
    char '['
    exprs <- sepBy expr (spaces' >> char ',' >> spaces')
    char ']'
    spaces'
    return $ ListExpr exprs

tupleExpr :: IParser Expr
tupleExpr = do
    char '('
    exprs <- sepBy expr (spaces' >> char ',' >> spaces')
    char ')'
    spaces'
    return $ TupleExpr exprs

recordExpr :: IParser Expr
recordExpr = do
  char '{'
  spaces'
  exprs <- sepBy recFieldAssign (spaces' >> char ',' >> spaces')
  spaces'
  char '}'
  spaces'
  return $ RecordExpr exprs
  where recFieldAssign = do
          name <- id_
          spaces'
          char '='
          spaces'
          e <- expr
          return (name, e)

lvalueExpr :: IParser LValueExpr
lvalueExpr = do
    name <- id_
    fs <- many (dot <|> brackets)
    spaces'
    return $ foldl (flip ($)) (VarExpr name) fs
  where dot = do
            char '.'
            spaces'
            name <- id_
            spaces'
            return (\x -> FieldAccessExpr x name)

        brackets = do
            char '['
            spaces'
            e <- expr
            spaces'
            char ']'
            spaces'
            return (\x -> ArrayAccessExpr x e)

lambdaExpr :: IParser Expr
lambdaExpr = do
    string "fn"
    spaces'
    args <- sepBy1 matchExpr spaces'
    spaces'
    string "=>"
    spaces
    sameOrIndented
    s <- block statement
    return $ LambdaExpr args (makeCompound s)

matchExpr :: IParser MatchExpr
matchExpr = do
    e <- matchExpr'
    tm <- maybeType
    spaces'
    return $ maybe e (\t -> TypedMatchExpr e t) tm
  where
    matchExpr' = tupleMatchExpr
             <|> listMatchExpr
             <|> recordMatchExpr
             -- 'try' since '-' will also be matched in a function declaration's return type signature
             <|> try intMatchExpr
             <|> stringMatchExpr
             <|> boolMatchExpr
             <|> varMatchExpr
             <|> parenthesisMatchExpr

parenthesisMatchExpr :: IParser MatchExpr
parenthesisMatchExpr = do
    char '('
    spaces'
    e <- matchExpr
    spaces'
    char ')'
    spaces'
    return e

tupleMatchExpr :: IParser MatchExpr
tupleMatchExpr = do
    char '('
    exprs <- sepBy matchExpr (spaces' >> char ',' >> spaces')
    char ')'
    spaces'
    return $ TupleMatchExpr exprs

listMatchExpr :: IParser MatchExpr
listMatchExpr = do
    char '['
    exprs <- sepBy matchExpr (spaces' >> char ',' >> spaces')
    char ']'
    spaces'
    return $ ListMatchExpr exprs

recordMatchExpr :: IParser MatchExpr
recordMatchExpr = matchRecordExpr
  where
    matchRecordExpr = do
        char '{'
        exprs <- sepBy recFieldAssign (spaces' >> char ',' >> spaces')
        char '}'
        spaces'
        return $ RecordMatchExpr exprs
    recFieldAssign = do
        name <- id_
        spaces'
        char '='
        spaces'
        e <- matchExpr
        return (name, e)

varMatchExpr :: IParser MatchExpr
varMatchExpr = do
    name <- id_
    spaces'
    return $ VarMatch name

intMatchExpr :: IParser MatchExpr
intMatchExpr = do
    n <- integer
    spaces'
    return $ IntMatchExpr n

stringMatchExpr :: IParser MatchExpr
stringMatchExpr = do
    s <- string_
    spaces'
    return $ StringMatchExpr s

boolMatchExpr :: IParser MatchExpr
boolMatchExpr = do
    b <- bool
    spaces'
    return $ BoolMatchExpr b

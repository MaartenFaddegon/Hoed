{-# LANGUAGE DeriveGeneric #-}
module TypedAst(module TypedAst, module Ast) where
import Ast
import Types
import qualified Data.List as List
import GHC.Generics
import Debug.Hoed.Pure

data TypedDecl = TTypeDecl String [String] Type
               | TFunDecl String [String] [TypedMatchExpr] Type TypedStatement
    deriving (Show)

data TExpr = TIntExpr Int
           | TRealExpr Double
           | TBoolExpr Bool
           | TStringExpr String
           | TOrExpr TypedExpr TypedExpr
           | TAndExpr TypedExpr TypedExpr
           | TEqExpr TypedExpr TypedExpr
           | TNeqExpr TypedExpr TypedExpr
           | TLtExpr TypedExpr TypedExpr
           | TGtExpr TypedExpr TypedExpr
           | TLeExpr TypedExpr TypedExpr
           | TGeExpr TypedExpr TypedExpr
           | TAddExpr TypedExpr TypedExpr
           | TSubExpr TypedExpr TypedExpr
           | TMultExpr TypedExpr TypedExpr
           | TDivExpr TypedExpr TypedExpr
           | TUnaryMinusExpr TypedExpr
           | TBangExpr TypedExpr
           | TCallExpr TypedExpr TypedExpr
           | TListExpr [TypedExpr]
           | TTupleExpr [TypedExpr]
           | TRecordExpr [(String, TypedExpr)]
           | TLValue TypedLValueExpr
           | TLambdaExpr [TypedMatchExpr] TypedStatement
  deriving Show

type TypedExpr = (TExpr, Type)

data TMatchExpr = TTupleMatchExpr [TypedMatchExpr]
                | TListMatchExpr [TypedMatchExpr]
                | TRecordMatchExpr [(String, TypedMatchExpr)]
                | TVarMatch String
                | TIntMatchExpr Int
                | TStringMatchExpr String
                | TBoolMatchExpr Bool
  deriving (Show, Eq, Ord, Generic)
instance Observable TMatchExpr

type TypedMatchExpr = (TMatchExpr, Type)

ppTypedMatchExpr :: TypedMatchExpr -> String
ppTypedMatchExpr (TTupleMatchExpr tmexprs, _) =
  "(" ++ List.intercalate ", " (List.map ppTypedMatchExpr tmexprs) ++ ")"
ppTypedMatchExpr (TListMatchExpr tmexprs, _) =
  "[" ++ List.intercalate ", " (List.map ppTypedMatchExpr tmexprs) ++ "]"
ppTypedMatchExpr (TRecordMatchExpr tmexprs, _) =
  "{" ++ List.intercalate ", " (List.map f tmexprs) ++ "}"
  where f (s, tmexpr) = s ++ " = " ++ ppTypedMatchExpr tmexpr
ppTypedMatchExpr (TVarMatch s, ty) = s ++ ": " ++ show ty
ppTypedMatchExpr (TIntMatchExpr n, _) = show n
ppTypedMatchExpr (TStringMatchExpr s, _) = "\"" ++ s ++ "\""
ppTypedMatchExpr (TBoolMatchExpr b, _) = show b

data TLValueExpr = TVarExpr String
                 | TFieldAccessExpr TypedLValueExpr String
                 | TArrayAccessExpr TypedLValueExpr TypedExpr
  deriving (Show)

type TypedLValueExpr = (TLValueExpr, Type)

data TypedStatement = TIfStatement TypedExpr TypedStatement (Maybe TypedStatement)
                    | TWhileStatement TypedExpr TypedStatement
                    | TForStatement TypedMatchExpr TypedExpr TypedStatement
                    | TCompoundStatement [TypedStatement]
                    | TAssignStatement (Either TypedMatchExpr TypedLValueExpr) TypedExpr
                    | TMatchStatement TypedExpr [(TypedMatchExpr, TypedStatement)]
                    | TReturnStatement TypedExpr
                    | TBreakStatement
                    | TContinueStatement
                    | TDeclStatement TypedDecl
  deriving (Show)

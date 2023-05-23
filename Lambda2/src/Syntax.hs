module Syntax where

import Types

-- AST Type
data Expr
    = Var Id
    | Lit (Id, Type)
    | Abs Id Type Expr
    | App Expr Expr

instance Show Expr where
    show (Var x) = x
    show (Lit (x,t)) = show t
    show (Abs x t e) = "(\\" ++ x ++ ":" ++ show t ++ ". " ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
module Syntax where

-- AST Type
data Expr
    = Var String
    | Abs String Expr
    | App Expr Expr

-- Pretty-print AST
instance Show Expr where
    show (Var v) = v
    show (Abs v e) = "\\" ++ v ++ "." ++ show e
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
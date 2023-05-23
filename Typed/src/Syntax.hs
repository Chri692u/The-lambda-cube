module Syntax where

-- Type environment
type Id = String
type TypeEnv = [(Id, Type)]

-- Function for showing TypeEnv
show' :: TypeEnv -> String
show' typeEnv = "{\n    " ++ showBindings typeEnv ++ "\n}"
    where
        showBindings [] = ""
        showBindings ((id, t) : xs) = id ++ " : " ++ show t ++ showTail xs
        showTail [] = ""
        showTail xs = ",\n    " ++ showBindings xs


-- Type Type
data Type 
    = TInt
    | TBool
    | TList Type
    | TArr Type Type
    deriving(Read, Eq)

instance Show Type where
    show TInt = "int"
    show TBool = "bool"
    show (TArr t1 t2) = show t1 ++ " -> " ++ show t2

-- Error types for printing
data TypeError = Mismatch Type Type
               | NotFunction Type
               | MixedList Type Type
               | NotInScope Id

instance Show TypeError where
  show (Mismatch t1 t2)  = "Type mismatch: type " ++ show t1 ++ " is not of type: " ++ show t2
  show (NotFunction t)  = "Type " ++ show t ++ " is not a abstraction. Lambda application can only be done on abstractions"
  show (NotInScope v1)   = "Variable " ++ show v1 ++ " is not in scope"
  show (MixedList t1 t2) = "List cannot have mixed types: " ++ show t1 ++ "and: " ++ show t2

-- AST Type
data Expr
    = Var Id
    | Lit (Id, Type)
    | Abs Id Type Expr
    | App Expr Expr
    deriving (Eq, Read)

instance Show Expr where
    show (Var x) = x
    show (Lit (x,t)) = show t
    show (Abs x t e) = "(\\" ++ x ++ ":" ++ show t ++ ". " ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
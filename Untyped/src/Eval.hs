module Eval(eval) where

import Syntax
import Data.List (nub)

-- Get the list of free variables for an expression
fv :: Expr -> [String]
fv (Var v) = [v]
fv (Abs v e) = filter (/= v) (fv e)
fv (App e1 e2) = nub (fv e1 ++ fv e2)

-- Return a fresh variable name that does not occur in the given expression
fresh :: Expr -> String
fresh e = head (filter (`notElem` vars) varNames)
  where vars = fv e
        varNames = [c:v | v <- "" : varNames, c <- ['a'..'z']]

-- Rename the bound variable in an abstraction (Alpha conversion)
rename :: String -> String -> Expr -> Expr
rename old new var@(Var v) = if v == old then Var new else var
rename old new (App e1 e2) = App (rename old new e1) (rename old new e2)
rename old new abs@(Abs v e)
    | v == old = abs
    | v `elem` fv (Var new) || v `elem` fv e =
        let v' = fresh e
            e' = rename v v' (rename old new e)
        in Abs v' e'
    | otherwise = Abs new (rename old new e)

-- Substitution of expressions
class Substitutable a where
  subst :: String -> a -> a -> a

instance Substitutable Expr where
    -- Substitution rule for variables
    subst name e' var@(Var v)
        | v == name = e'
        | otherwise    = var

    -- Substitution rule for abstraction
    subst name e' abs@(Abs v e)
        | v == name = abs
        | v `elem` fv e' = 
            let v' = fresh e'
                e'' = rename v v' e
            in Abs v' (subst name e' e'')
        | otherwise    = Abs v (subst name e' e)

    -- Substitution rule for application
    subst name e' (App e1 e2) =
        App (subst name e' e1) (subst name e' e2)

-- Top level (Beta reductions)
eval :: Expr -> Expr
eval (Var v) = Var v
eval (Abs v e) = Abs v (eval e)
eval (App e1 e2) = 
    case eval e1 of
        Abs v e1' -> eval (subst v e2 e1')
        e1' -> App e1' (eval e2)
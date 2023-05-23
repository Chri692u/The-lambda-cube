module TypeSystem where

import Syntax
import Control.Monad.Except
import Control.Monad.Reader

type CheckM a = ExceptT TypeError (Reader TypeEnv) a 

-- Extend type environment
extend :: (Id, Type) -> CheckM a -> CheckM a
extend (x,t) = local (extendTEnv (x,t))

extendTEnv :: (Id, Type) -> TypeEnv -> TypeEnv
extendTEnv xt env = xt : env

-- Type judgement
judgement :: Id -> CheckM Type
judgement x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

-- Type of valid typed lambda terms
typeof :: Expr -> CheckM (TypeEnv, Type)
typeof (Lit (x,t)) = do
  env <- ask
  return (env, t)

typeof (Var var) = do
  env <- ask
  t <- judgement var
  return (env, t)

typeof (Abs var t e) = do
    (env', rhs) <- extend (var, t) (typeof e)
    return (env', TArr t rhs)

typeof (App e1 e2) = do
    (env1, t1) <- typeof e1
    (env2, t2) <- typeof e2
    case t1 of
        (TArr a b) | a == t2 -> return (env1, b)
                   | otherwise -> throwError $ Mismatch t2 a
        ty -> throwError $ NotFunction ty

-- Run type checking algortihm
runCheck :: TypeEnv -> CheckM a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

-- Top level
typecheck :: TypeEnv -> Expr -> Either TypeError (TypeEnv, Type)
typecheck env x = runCheck env $ typeof x

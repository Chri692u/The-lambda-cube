module TypeSystem where

import Syntax
import Types
import Control.Monad.Except
import Control.Monad.Reader

type CheckM a = ExceptT TypeError (Reader TypeEnv) a

-- Extend type environment
extend :: (Id, Type) -> CheckM a -> CheckM a
extend (x,t) = local (extendTEnv (x,t))

extendTEnv :: (Id, Type) -> TypeEnv -> TypeEnv
extendTEnv xt env = xt : env

-- Type judgement
tLookup :: Id -> CheckM Type
tLookup x = do
    env <- ask
    case lookup x env of
        Just e  -> return e
        Nothing -> throwError $ NotInScope x

-- Type judgement
judgement :: Id -> CheckM Bool
judgement x = do
    env <- ask
    case lookup x env of
        Just e  -> return True
        Nothing -> return False

-- Type of valid typed Lambda2 terms
typeof :: Expr -> CheckM (TypeEnv, Type)
typeof (Lit (x,t)) = do
    env <- ask
    return (env, t)

typeof (Var var) = do
    env <- ask
    t <- tLookup var
    return (env, t)

typeof (Abs var t e) = undefined
-- Tjek om t updatere contexten
-- Hvis t er en TArr t1 t2, så:
--    tjek om t1 og t2 introducere type variabler til context
--    tjek om mulige type variable t1 og t2 er introduceret
--    returner updateret env og TArr t t2

-- Hvis vi har en TPi skal vi opdatere contexten med det type variable som bliver lavet

typeof (App e1 e2) = undefined
-- Hvis typen af e1 ikke er TArr, så smid en fejl
-- Ellers, så se om a > t2 hvor (TArr a b)
-- Hvor a > t2, f.eks så * > bool, * > int og ? > bool men ikke ? > int

-- Run type checking algorithm
runCheck :: TypeEnv -> CheckM a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

-- Top level
typecheck :: TypeEnv -> Expr -> Either TypeError (TypeEnv, Type)
typecheck env x = runCheck env $ typeof x
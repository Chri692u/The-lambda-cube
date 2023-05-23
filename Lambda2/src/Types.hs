module Types where

newtype TVar = TV String
    deriving (Eq, Ord)

instance Show TVar where
    show (TV x) = x

tvarName :: TVar -> String
tvarName (TV var) = var

data Kind
    = Star
    | Other
    deriving (Eq, Ord)

instance Show Kind where
    show Star = "*"
    show Other = "?"

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  | TPi TVar Kind

infixr `TArr`

instance Show Type where
    show (TVar x) = show x
    show (TCon x) = x
    show (TArr t1 t2) = show t1 ++ " -> " ++ show t2
    show (TPi x k) = "(" ++ show x ++ ":" ++ show k ++ ")"

-- Error types for printing
data TypeError = Mismatch Type Type
               | NotFunction Type
               | NoTypeVariable Id
               | NotInScope Id
               | BoundTypeVariable Type

instance Show TypeError where
  show (Mismatch t1 t2)  = "Type mismatch: type " ++ show t1 ++ " is not of type: " ++ show t2 ++ "."
  show (NotFunction t)  = "Type " ++ show t ++ " is not a abstraction."
  show (NotInScope v1)   = "Variable " ++ show v1 ++ " is not in scope."
  show (NoTypeVariable v) = "Type variable " ++ v ++ " is not bound to a type."
  show (BoundTypeVariable t) = "Type variable is already bound: " ++ show t 

-- Type environment
type Id = String

-- Type variable environment
type TypeEnv = [(Id, Type)]

-- Function for showing TypeEnv
show' :: TypeEnv -> String
show' typeEnv = "{\n    " ++ showBindings typeEnv ++ "\n}"
    where
        showBindings [] = ""
        showBindings ((id, t) : xs) = id ++ " : " ++ show t ++ showTail xs
        showTail [] = ""
        showTail xs = ",\n    " ++ showBindings xs

typeInt :: Type
typeInt  = TCon "int"

typeBool :: Type
typeBool = TCon "bool"
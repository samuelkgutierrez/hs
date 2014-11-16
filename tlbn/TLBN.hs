module TLBN where

-- Terms
data Term = TrmVar String -- varName
          | TrmTru -- true
          | TrmFls -- false
          | TrmIf Term Term Term -- if t1 then t2 else t3 fi
          | TrmZero -- 0
          | TrmSucc Term -- succ T
          | TrmPred Term -- pred T
          | TrmIsZero Term   -- iszero T
          | TrmApp Term Term -- Function Application
          | TrmAbs String Type Term
          | TrmBind String Binding
          deriving Eq

-- Implements how we show Terms.
instance Show Term where
    -- Shows true
    show TrmTru = "True"
    -- Shows false
    show TrmFls = "False"
    -- Shows 0
    show TrmZero = "0"
    -- Shows succ
    show (TrmSucc t) | isNumericValue t = show $ returnNumericValue (TrmSucc t)
                     | otherwise = "(succ " ++ show t ++ ")"
    -- Shows pred
    show (TrmPred t) = "(pred " ++ show t ++ ")"
    -- Shows iszero
    show (TrmIsZero t) = "(iszero " ++ show t ++ ")"
    -- Show if statements
    show (TrmIf cond thn el) =
        "if (" ++ show cond ++ ") then " ++ show thn ++ " else " ++ show el
    -- Shows variables.
    show (TrmVar s) = s
    -- Shows lamda abstraction terms.
    show (TrmAbs name typ body) =
        "abs (" ++ name ++ ":" ++ show typ ++ " . " ++ show body ++ ")"
    -- Shows function application.
    show (TrmApp tFn tArg) =
        "app (" ++ show tFn ++ " ,\n     " ++ show tArg ++ ")"
    -- Catch-all
    show _ = error "Don't know how to show given Term :-("

-- Types
data Type = TyArr Type Type
          | TyBool
          | TyNat
          deriving Eq

-- Implements how we show Types.
instance Show Type where
    show TyBool = "Bool"
    show TyNat  = "Nat"
    show _ = error "Unsupported Type in Show"

returnNumericValue :: Term -> Integer
returnNumericValue TrmZero = 0
returnNumericValue (TrmSucc t) = returnNumericValue t + 1
returnNumericValue _ = error "Invalid Term in returnNumericValue"

-- Retruns whether or not a given term is a boolean value.
isBooleanValue :: Term -> Bool
isBooleanValue trm = case trm of
    TrmTru -> True
    TrmFls -> True
    _      -> False

-- Retruns whether or not a given term is a numerical value.
isNumericValue :: Term -> Bool
isNumericValue trm = case trm of
    TrmZero     -> True
    (TrmSucc t) -> isNumericValue t
    _           -> False

-- Retruns whether or not a given term is an abstraction value.
isAbstractionValue :: Term -> Bool
isAbstractionValue trm = case trm of
    TrmAbs {} -> True
    _         -> False

-- Retruns whether or not a given term is a value in our language.
isValue :: Term -> Bool
isValue t = isNumericValue t || isBooleanValue t || isAbstractionValue t

-- Bindings
data Binding = NameBind
             | TyVarBind
             | VarBind Type
             | TmAbbBind Term (Maybe Type)
             | TyAbbBind Type
               deriving (Show, Eq)

-- Typing
-- Convenience routine for printing out type error messages.
typeErrorComplain :: (Show a1, Show a) => a -> a1 -> t
typeErrorComplain exTy gotTy =
    error ("Type error detected. Expected '"
                ++ show exTy ++ "', but got '"
                ++ show gotTy ++ "'.")

checkType :: Term -> Type -> Type -> Type
checkType t exTy ouTy = do
    let typeOfT = typeof t
    if exTy == typeOfT
    then ouTy
    else typeErrorComplain exTy typeOfT

-- Implements the code responsible for calculating the type of a given term.
typeof :: Term -> Type
-- Typing for If statements.
typeof (TrmIf c t e) = do
    let cTy = typeof c
    if TyBool /= cTy
    then typeErrorComplain TyBool cTy
    else do
         -- Make sure both branches are of the same type.
         let tyT = typeof t
         let tyE = checkType e tyT tyT
         -- If so, then just use the type of the else branch.
         tyE
-- Typing for abstractions.
-- The rest.
typeof trm = case trm of
    TrmFls        -> TyBool
    TrmTru        -> TyBool
    TrmZero       -> TyNat
    (TrmSucc t)   -> checkType t TyNat TyNat
    (TrmPred t)   -> checkType t TyNat TyNat
    (TrmIsZero t) -> checkType t TyNat TyBool
    t@_ -> error ("Cannot determ term type of the following:\n" ++ show t)

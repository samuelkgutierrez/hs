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
    show TrmTru      = "True"
    -- Shows false
    show TrmFls      = "False"
    -- Shows 0
    show TrmZero     = "0"
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
        "app (" ++ show tFn ++ " , " ++ show tArg ++ ")"
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

isNumericValue :: Term -> Bool
isNumericValue trm = case trm of
    TrmZero     -> True
    (TrmSucc t) -> isNumericValue t
    _           -> False

data Binding = NameBind
             | TyVarBind
             | VarBind Type
             | TmAbbBind Term (Maybe Type)
             | TyAbbBind Type
               deriving (Show, Eq)

%include polycode.fmt
\section{TLBN}

\noindent
Describes the TLBN language and implements routines that show Terms, Types, and
Bindings. Other helper routines are implemented here that facilitate these
operations.

\begin{code}
module TLBN where

-- Terms
data Term = TrmVar Int String -- variable
          | TrmTru -- true
          | TrmFls -- false
          | TrmIf Term Term Term -- if t1 then t2 else t3 fi
          | TrmFix Term -- fix T
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
    -- Shows succ. For convenience, we show the decimal value if we are able.
    show (TrmSucc t) | isNumericValue t = show $ returnNumericValue (TrmSucc t)
                     | otherwise = "(succ " ++ show t ++ ")"
    -- Shows pred
    show (TrmPred t) = "(pred " ++ show t ++ ")"
    -- Shows iszero
    show (TrmIsZero t) = "(iszero " ++ show t ++ ")"
    -- Show if
    show (TrmIf cond thn el) =
        "if (" ++ show cond ++ ") then " ++ show thn ++ " else " ++ show el
    -- Shows variables.
    show (TrmVar _ n) = n
    -- Shows lamda abstraction terms.
    show (TrmAbs name typ body) =
        "abs (" ++ name ++ ":" ++ show typ ++ " . " ++ show body ++ ")"
    -- Shows function application.
    show (TrmApp tFn tArg) =
        "app (" ++ show tFn ++ " ," ++ show tArg ++ ")"
    -- Shows fix.
    show (TrmFix t) =
        "fix (" ++ show t ++ ")"
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
    show (TyArr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

-- Bindings
data Binding = TyVarBind
             | VarBind Type
               deriving (Show, Eq)
\end{code}
\noindent
Returns the decimal representation of a Term with Nat type. Errors if provided a
term that cannot be converted into an Integer.
\begin{code}
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
\end{code}

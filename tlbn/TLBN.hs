module TLBN where

-- Terms
data Term = TrmIdent String
          | TrmTru
          | TrmFls
          | TrmIf Term Term Term
          | TrmZero
          | TrmSucc Term
          | TrmPred Term
          | TrmIsZero Term   -- iszero T
          | TrmApp Term Term -- Function Application
          | TrmAbs Term Type Term
          deriving Eq

-- Types
data Type = TyArr Type Type
          | TyBool
          | TyNat
          deriving Eq

-- Implements how we show Terms.
instance Show Term where
    show TrmTru      = "True"
    show TrmFls      = "False"
    show TrmZero     = "0"
    show (TrmSucc t) | isNumericValue t = show $ returnNumericValue (TrmSucc t) 
                     | otherwise = show "(succ " ++ show t ++ ")"
    show (TrmPred t) = "(pred " ++ show t ++ ")"
    show (TrmIsZero t) = "(iszero " ++ show t ++ ")"
    show (TrmIf cond thn el) = "if (" ++ show cond ++ ") then "
                             ++ show thn
                             ++ " else " ++ show el
    show (TrmIdent s) = show s
    show (TrmAbs name _ _) = show "abs (" ++ show name ++ ":"
    show _ = error "Don't know how to show given Term"

returnNumericValue :: Term -> Integer
returnNumericValue TrmZero = 0
returnNumericValue (TrmSucc t) = returnNumericValue t + 1
returnNumericValue _ = error "Invalid Term"

isNumericValue :: Term -> Bool
isNumericValue trm = case trm of
    TrmZero     -> True
    (TrmSucc t) -> isNumericValue t
    _           -> False

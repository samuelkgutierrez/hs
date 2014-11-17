module Typing (typeof) where

import TLBN

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
-- Typing for variables
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

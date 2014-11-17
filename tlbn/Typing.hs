module Typing (termType) where

import TLBNError
import TLBN
import Context

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

typeErrorComplain exTy gotTy =
    throwError $ Default ("Type error detected. Expected '"
                ++ show exTy ++ "', but got '"
                ++ show gotTy ++ "'.")

checkType :: Term -> Type -> b -> ErrorT TLBNError (State Context) b
checkType t exTy ouTy = do
    typeOfT <- typeof t
    if exTy == typeOfT
    then return ouTy
    else typeErrorComplain exTy typeOfT

typeOfBinding :: Binding -> ContextThrowsError Type
typeOfBinding (VarBind ty) = return ty
typeOfBinding _ = throwError $ Default "Cannot determine type of binding"

-- Variable typing
typeof :: Term -> ContextThrowsError Type
typeof (TrmVar idx _) = do
    ctx <- get
    b <- liftThrows $ bindingOf idx ctx
    typeOfBinding b
---- The rest.
typeof trm = case trm of
    TrmTru  -> return TyBool
    TrmFls  -> return TyBool
    TrmZero -> return TyNat
    (TrmSucc t) -> checkType t TyNat TyNat
    (TrmPred t) -> checkType t TyNat TyNat
    (TrmIsZero t) -> checkType t TyNat TyBool

termType :: Term -> ThrowsError Type
termType  = runContextThrows . typeof

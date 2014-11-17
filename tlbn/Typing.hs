module Typing (termType, termType') where

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

typeof :: Term -> ContextThrowsError Type
typeof TrmTru = return TyBool
typeof TrmFls = return TyBool
typeof TrmZero = return TyNat
typeof (TrmSucc t) = checkType t TyNat TyNat
typeof (TrmPred t) = checkType t TyNat TyNat
typeof (TrmIsZero t) = checkType t TyNat TyBool
-- If statement typing
typeof (TrmIf c t e) = do
    tyC <- typeof c
    if tyC /= TyBool
    then typeErrorComplain TyBool tyC
    else do
        tyT <- typeof t
        checkType e tyT tyT
-- Type of bind term
typeof (TrmBind _ binding) = typeOfBinding binding
-- Function application typing
typeof (TrmApp t1 t2) = do
    tyT1 <- typeof t1
    tyT2 <- typeof t2
    case tyT1 of
      (TyArr _ _) -> checkTyArr tyT1 tyT2
      otherwise -> throwError $ Default "Abstraction expected."
    where checkTyArr (TyArr tyArr1 tyArr2) tyT2
              | subtype tyT2 tyArr1 = return tyArr2
              | otherwise           = throwError $ Default ":-("
-- Abstraction typing
typeof (TrmAbs var ty body) =
    withBinding var (VarBind ty) $ liftM (TyArr ty) $ typeof body
-- Variable typing
typeof (TrmVar idx _) = do
    ctx <- get
    b <- liftThrows $ bindingOf idx ctx
    typeOfBinding b

termType :: Term -> ThrowsError Type
termType  = runContextThrows . typeof

termType' :: Monad m => Term -> m Type
termType' ty = return $ extractValue $ runContextThrows $ typeof ty

subtype :: Type -> Type -> Bool
subtype = (==)

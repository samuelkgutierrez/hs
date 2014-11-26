%include polycode.fmt
\section{Typing}

\noindent
Implements typing routines for TLBN.

\begin{code}
module Typing (termType, termType', getType) where

import TLBNError
import TLBN
import Context

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Error

-- Helper function that is used when throwing a type error.
typeErrorComplain :: (Show a2, Show a1, Monad m) =>
                     a1 -> a2 -> ErrorT TLBNError m a
typeErrorComplain exTy gotTy =
    throwError $ Default ("Type error detected. Expected '"
                ++ show exTy ++ "', but got '"
                ++ show gotTy ++ "'.")
\end{code}

\noindent
Helper function that is used to check the type of a Term against an expected
type. Returns the expected return type if a typing error is not encountered.
\begin{code}
checkType :: Term -> Type -> b -> ErrorT TLBNError (State Context) b
checkType t exTy ouTy = do
    typeOfT <- typeof t
    if exTy == typeOfT
    then return ouTy
    else typeErrorComplain exTy typeOfT
\end{code}

\noindent
Returns the type of a given Binding if an error is not encountered.
\begin{code}
typeOfBinding :: Binding -> ContextThrowsError Type
typeOfBinding (VarBind ty) = return ty
typeOfBinding _ = error "Cannot determine type of binding"

-- Typing for simple Terms.
typeof :: Term -> ContextThrowsError Type
typeof TrmTru = return TyBool
typeof TrmFls = return TyBool
typeof TrmZero = return TyNat
typeof (TrmSucc t) = checkType t TyNat TyNat
typeof (TrmPred t) = checkType t TyNat TyNat
typeof (TrmIsZero t) = checkType t TyNat TyBool
-- If typing
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
    -- Make sure that the first argument is a function type.
         (TyArr _ _) -> checkTyArr tyT1 tyT2
         -- Not a function type, so complain.
         t -> throwError $ Default ("For 1st argument in app -- Expected "
                                    ++ "'TyArr', but got '" ++ show t ++ "'.")
    -- Now make sure that the argument type is the same as the function expects.
    where checkTyArr tArr tyT2 = case tArr of
            (TyArr tyArr1 tyArr2) -> if tyT2 == tyArr1
                                     then return tyArr2
                                     else typeErrorComplain tyArr2 tyT2
            -- Given our construction, this should never happen.
            _                     -> error "Badness!"
-- Abstraction typing
typeof (TrmAbs var ty body) =
    withBinding var (VarBind ty) $ liftM (TyArr ty) $ typeof body
-- Variable typing
typeof (TrmVar _ idx _) = do
    ctx <- get
    b <- liftThrows $ bindingOf idx ctx
    typeOfBinding b
\end{code}

\noindent
Fix typing. First get the type of fix's term. The typing relation dictates that
the type of fix t will be $T_1$ if $t_1$ has type $(T_1 \rightarrow T_1)$
\begin{code}
typeof (TrmFix t) = do
    -- First get the type of t. This needs to be an arrow type.
    tyT <- typeof t
    case tyT of
        -- Good, this is an arrow type. Make sure that the types on either side
        -- of the arrow are the same.
        (TyArr ty1 ty2) -> if ty1 == ty2
                           then return ty1 -- The same, so pick one.
                           else error ("Type mismatch found in fix term. "
                                       ++ "Type on either side of arrow aren't "
                                       ++ "the same. Got '"
                                       ++ show tyT ++ "'")
        -- If not an arrow type, complain.
        _               -> error ("Expected arrow type in fix, but got: '"
                                  ++ show tyT ++ "'")

\end{code}

\noindent
TODO
\begin{code}
getType :: t -> Type -> (t -> Term -> Term) -> Type
getType c ty f = case ty of
    TyArr ty1 ty2 -> TyArr (getType c ty1 f)
                     (getType c ty2 f)
    TyVar v -> TyVar $ f c v
    _ -> ty
\end{code}

\noindent
Top-level call that attempts to type a given Term.
\begin{code}
termType :: Term -> ThrowsError Type
termType  = runContextThrows . typeof

-- An alternative implementation of termType with a different return type.
termType' :: Term -> ThrowsError a -> a
termType' ty = return extractValue $ runContextThrows $ typeof ty
\end{code}

%include polycode.fmt
\section{Evaluator}

\noindent
An implementation of an evaluator based on the small-step evaluation relation
for the language of booleans \texttt{Bool} and natural numbers \texttt{Nat} that
closely as possible follows the specified behavior. 

\begin{code}
module Evaluator (evalTerm) where

import TLBN
import qualified Control.Monad as CMonad (liftM)
import qualified Data.Maybe as DMaybe (fromMaybe)
\end{code}

\begin{code}
-- Implements a one step evaluation relation.
eval1 :: Term -> Maybe Term
-- Succ
eval1 (TrmSucc t) = CMonad.liftM TrmSucc (eval1 t)
-- Pred
eval1 (TrmPred TrmZero) = Just TrmZero -- No negative values in this language.
eval1 (TrmPred (TrmSucc t)) = Just t
eval1 (TrmPred t) = CMonad.liftM TrmPred (eval1 t)
-- IsZero
eval1 (TrmIsZero TrmZero) = Just TrmTru
eval1 (TrmIsZero (TrmSucc _)) = Just TrmFls
eval1 (TrmIsZero t) = CMonad.liftM TrmIsZero (eval1 t)
-- If
eval1 (TrmIf TrmTru thn _) = Just thn
eval1 (TrmIf TrmFls _ el) = Just el
eval1 (TrmIf cond thn el) = CMonad.liftM (\x -> TrmIf x thn el) (eval1 cond)
-- Application
-- Application of abstraction
eval1 (TrmApp t1@(TrmAbs _ _ body) t2)
    -- if t2 is a value, then just replace its value inside of t1's body
    | isValue t2 = Just (tSub body t2)
    -- t2 is not a value, so eval1 t2
    | otherwise = CMonad.liftM (TrmApp t1) (eval1 t2)
-- More general Application
eval1 (TrmApp t1 t2)
     -- if t1 is not a value and not a lambda, then eval1 t1
     | not $ isValue t1 = CMonad.liftM (`TrmApp` t2) (eval1 t1)
-- Evaluation relations for fix.
-- Implements E-FIX. Simply eval1 t to get t' if t is not a lambda.
eval1 (TrmFix t)
    | not $ isValue t = CMonad.liftM TrmFix (eval1 t)
-- Implements E-FIXBETA. In this case, t is a lambda abstraction.
eval1 t@(TrmFix (TrmAbs _ _ body)) = Just (tSub body t)
-- Signifies that the provided term is not in our language or is a normal form.
eval1 _ = Nothing

updateTerm :: Num t => t -> Term -> (t -> Term -> Term) -> Term
updateTerm c t f = case t of
    TrmSucc ts        -> TrmSucc   (continue ts)
    TrmPred tp        -> TrmPred   (continue tp)
    TrmIsZero tn      -> TrmIsZero (continue tn)
    TrmFix tf         -> TrmFix    (continue tf)
    TrmIf cnd thn els -> TrmIf     (continue cnd)
                                   (continue thn)
                                   (continue els)
    TrmApp fn a       -> TrmApp    (continue fn)
                                   (continue a)
    TrmAbs var ty body -> TrmAbs var (walkType c ty f)
                          (updateTerm (c + 1) body f)
    TrmVar {} -> f c t
    _ -> t
    where continue = (\tb -> updateTerm c tb f)

walkType c ty f = case ty of
                    TyVar v -> TyVar $ f c v
                    TyArr ty1 ty2 -> TyArr (walkType c ty1 f)
                                     (walkType c ty2 f)
                    otherwise -> ty

sub i val t = updateTerm 0 t subVar
    where subVar c v@(TrmVar idx _) | c + i == idx = shift c val
                                   | otherwise    = v

shift i t = updateTerm 0 t shiftVar
    where shiftVar c (TrmVar idx ctxLen)
              | idx >= c  = TrmVar (idx + i) (ctxLen + i)
              | otherwise = TrmVar idx (ctxLen + i)

-- Given a target body and a term that represents the
tSub :: Term -> Term -> Term
tSub body repl = shift (-1) $ sub 0 (shift 1 repl) body
\end{code}

\noindent
Implements a multi-step relation that iterates \texttt{eval1} as many times as
possible.  If t' == t then we know that we are either done or that we are stuck,
so just return t and don't call \texttt{eval} on t'. Else, we know that we've
made progress and can try to further evaluate one more time.
\begin{code}
eval :: Term -> Term
eval t = let t' = DMaybe.fromMaybe t (eval1 t)
         in if t' == t then t else eval t'
\end{code}

\noindent
Term evaluation for use in monadic situations. Just a thin wrapper around eval.
\begin{code}
evalTerm :: Monad m => Term -> m Term
evalTerm ts = return (eval ts)
\end{code}

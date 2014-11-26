%include polycode.fmt
\section{Evaluator}

\noindent
An implementation of an evaluator based on the small-step evaluation relation
for the language of booleans \texttt{Bool} and natural numbers \texttt{Nat} that
closely as possible follows the behavior presented in \texttt{TAPL}.

\begin{code}
module Evaluator (evalTerm) where

import TLBN
import qualified Control.Monad as CMonad (liftM)
import qualified Data.Maybe as DMaybe (fromMaybe)

\end{code}

\noindent
Implements variable substitution for terms in our language. Helper routine
called by varSub.
\begin{code}
doVarSub :: Term -> Term -> Term
doVarSub body repl = case body of
    TrmTru         -> TrmTru
    TrmFls         -> TrmFls
    TrmZero        -> TrmZero
    (TrmVar {})    -> repl
    (TrmSucc t)    -> TrmSucc (again t repl)
    (TrmPred t)    -> TrmPred (again t repl)
    (TrmIsZero t)  -> TrmIsZero (again t repl)
    (TrmIf c t e)  -> TrmIf (again c repl) (again t repl) (again e repl)
    (TrmApp f a)   -> TrmApp f (again a repl)
    (TrmFix t)     -> TrmFix (again t repl) 
    pt@_           -> error ("Cannot perform variable substitution on: '"
                             ++ show pt ++ "'")
    where again = doVarSub
\end{code}

\noindent
Implements variable substitution within a lambda abstraction. Given an abs and a
term, returns a new term with
\begin{code}

varSub :: Term -> Term -> Term
varSub (TrmAbs _ _ body) repl = doVarSub body repl
varSub t1@_ t2@_ = error ("Invalid top-level call to varSub.\n"
                          ++ "T1: '" ++ show t1 ++ "'\n"
                          ++ "TR: '" ++ show t2 ++ "'")

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
eval1 (TrmApp t1@(TrmAbs {}) t2)
    -- if t2 is a value, then just replace its value inside of t1's body
    | isValue t2 = Just (varSub t1 t2)
    -- t2 is not a value, so eval1 t2
    | otherwise = CMonad.liftM (TrmApp t1) (eval1 t2)
-- More general Application
eval1 (TrmApp t1 t2)
     -- if t1 is not a value and not a lambda, then eval1 t1
     | not $ isValue t1 = CMonad.liftM (`TrmApp` t2) (eval1 t1)
-- Evaluation relations for fix.
-- Implements E-FIX. Simply eval1 t to get t' if t is not a \.
eval1 (TrmFix t)
    | not $ isValue t = CMonad.liftM TrmFix (eval1 t)

-- Implements E-FIXBETA. In this case, t is a lambda abstraction.
eval1 (TrmFix t@(TrmAbs label typ body)) =
    Just $ doVarSub body (TrmFix body)

-- Signifies that the provided term is not in our language or is a normal form.
eval1 _ = Nothing
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

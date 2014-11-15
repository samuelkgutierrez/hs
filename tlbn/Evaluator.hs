module Evaluator (eval) where
-- An implementation of an evaluator based on the small-step evaluation relation
-- for the language of booleans \texttt{Bool} and natural numbers \texttt{Nat}
-- that closely as possible follows the behavior presented in \texttt{TAPL}.

import TLBN
import qualified Control.Monad as CMonad (liftM)
import qualified Data.Maybe as DMaybe (fromMaybe)

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
-- Not in our language
eval1 _ = Nothing -- Signifies that the provided term is not in our language.

-- Implements a multi-step relation that iterates \texttt{eval1} as many times as
-- possible.  If t' == t then we know that we are either done or that we are stuck,
-- so just return t and don't call \texttt{eval} on t'. Else, we know that we've made
-- progress and can try to further evaluate one more time.
eval :: Term -> Term
eval t = let t' = DMaybe.fromMaybe t (eval1 t)
         in if t' == t then t else eval t'

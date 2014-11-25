%include polycode.fmt
\section{TLBNShow}

\noindent
Implements showResults and helper routines for showing Terms and Types inside of
showResults.

\begin{code}
module TLBNShow (showResults) where

import TLBN
import TLBNError
import Context
import Control.Monad.Writer
import Control.Monad.State

type Printer = WriterT String ContextThrowsError

-- Runs printer.
runPrinter :: Printer () -> ThrowsError String
runPrinter = runContextThrows . execWriterT

-- Shows a variable name given a variable ID.
showVar :: Int -> Printer ()
showVar idx = do
    ctx <- get
    name <- lift . liftThrows $ nameOf idx ctx
    tell name
\end{code}

\noindent
Shows Terms in a way that is convenient for use inside of showResults. Most of
the heavy lifting is done within show Term.
\begin{code}
showTerm :: Term -> Printer ()
showTerm TrmTru = tell "true"
showTerm TrmFls = tell "false"
showTerm TrmZero = tell "0"
showTerm (TrmSucc t) = tell $ show (TrmSucc t)
showTerm (TrmPred t) = tell $ show (TrmPred t)
showTerm (TrmIsZero t) = tell $ show (TrmIsZero t)
showTerm (TrmIf c t e) = tell $ show (TrmIf c t e)
showTerm (TrmVar i _) = showVar i
showTerm (TrmAbs n t b) = tell $ show (TrmAbs n t b)
showTerm (TrmApp f a) = tell $ show (TrmApp f a)
showTerm _ = tell "Trm?"
\end{code}

\noindent
Shows type of provided Type that is convenient for use inside of showResults.
\begin{code}
showType :: Type -> Printer ()
showType TyBool = tell $ show TyBool
showType TyNat = tell $ show TyNat
showType (TyArr t1 t2) = tell $ show (TyArr t1 t2)
\end{code}

\noindent
Given an array of unevaluated terms, corresponding types, and normal forms,
prints out relavant information regarding the respective function arguments.
\begin{code}
showResults :: [Term] -> [Type] -> [Term] -> ThrowsError String
showResults ts tys nfs = do
    runPrinter . mapM_ showLine $ zip3 ts tys nfs
        where showLine (t,ty,nf) = do
              tell "-- Term: --\n"
              showTerm t
              origCtx <- get
              tell "\n-- Type: --\n"
              withContext origCtx $ showType ty
              tell "\n-- Normal Form: --\n"
              tell $ show nf
              tell "\n"
\end{code}

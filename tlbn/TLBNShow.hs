module TLBNShow (showResults) where

-- Implements showResults and helper routines for showing Terms and Types inside
-- of showResults.

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

-- Shows Terms in a way that is convenient for use inside of showResults. Most
-- of the heavy lifting is done within show Term.
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

-- Shows type of provided Type that is convenient for use inside of showResults.
showType :: Type -> Printer ()
showType TyBool = tell $ show TyBool
showType TyNat = tell $ show TyNat
showType (TyArr t1 t2) = tell $ show (TyArr t1 t2)

-- Given an array of unevaluated terms, corresponding types, and normal forms,
-- prints out relavant information regarding the respective function arguments.
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

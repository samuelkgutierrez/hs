module TLBNShow where

import TLBN
import TLBNError
import Context
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

type Printer = WriterT String ContextThrowsError

runPrinter :: Printer () -> ThrowsError String
runPrinter = runContextThrows . execWriterT

showVar :: Int -> Printer ()
showVar idx = do
    ctx <- get
    name <- lift . liftThrows $ nameOf idx ctx
    tell name

showTerm :: Term -> Printer ()
showTerm TrmTru = tell "true"
showTerm TrmFls = tell "false"
showTerm TrmZero = tell "0"
showTerm (TrmSucc t) = tell $ show (TrmSucc t)
showTerm (TrmPred t) = tell $ show (TrmPred t)
showTerm (TrmIsZero t) = tell $ show (TrmIsZero t)
showTerm (TrmIf c t e) = tell $ show (TrmIf c t e)
showTerm (TrmVar i n) = tell $ show (TrmVar i n)
showTerm (TrmAbs n t b) = tell $ show (TrmAbs n t b)
showTerm (TrmApp f a) = tell $ show (TrmApp f a)

showTerms :: Term -> ThrowsError String
showTerms trm = runPrinter $ shwTrm trm
    where shwTrm t = do
          tell "-- Term: --\n"
          showTerm t

showTypes :: Type -> ThrowsError String
showTypes ty = runPrinter $ shwTy ty
    where shwTy t = do
          tell "-- Type: --\n"
          tell $ show t

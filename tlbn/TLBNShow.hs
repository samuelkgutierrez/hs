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

showType :: Type -> Printer ()
showType TyBool = tell $ show TyBool
showType TyNat = tell $ show TyNat
showType (TyArr t1 t2) = tell $ show (TyArr t1 t2)

showTypeOfTerm :: Term -> Type -> Printer ()
showTypeOfTerm _ ty = showType ty

showTerms :: Term -> ThrowsError String
showTerms trm = runPrinter $ shwTrm trm
    where shwTrm t = do
          tell "-- Term: --\n"
          showTerm t

showResults :: [Term] -> [Type] -> [Term] -> ThrowsError String
showResults ts tys nfs = do
    runPrinter . mapM_ showLine $ zip3 ts tys nfs
        where showLine (t,ty,nf) = do origCtx <- get
                                      tell "-- Term: --\n"
                                      showTerm t
                                      tell "\n-- Type: --\n"
                                      withContext origCtx $ showTypeOfTerm t ty
                                      tell "\n-- Normal Form: --\n"
                                      tell $ show nf
                                      tell "\n"

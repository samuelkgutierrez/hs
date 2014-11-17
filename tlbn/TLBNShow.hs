module TLBNShow where

import TLBN
import TLBNError
import Context
import Control.Monad.State
import Control.Monad.Writer


type Printer = WriterT String ContextThrowsError

runPrinter :: Printer () -> ThrowsError String
runPrinter = runContextThrows . execWriterT

showTerm :: Term -> Printer ()
showTerm TrmTru  = tell "true"
showTerm TrmFls  = tell "false"
showTerm TrmZero = tell "0"

showTerms :: Term -> ThrowsError String
showTerms trm = runPrinter $ shwTrm trm
    where shwTrm t = do
          tell "-- Term: --\n"
          showTerm t


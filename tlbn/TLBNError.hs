module TLBNError where

-- Adapted from TAPL fullsimple error code.
-- Implements error routines for TLBN.

import Control.Monad.Except as CMonadEx
import Control.Monad.Trans.Error

data TLBNError = Default String
               
instance Show TLBNError where
    show (Default msg) = "Error: " ++ msg

instance Error TLBNError where
    noMsg  = Default "An unknown error has occurred."
    strMsg = Default

type ThrowsError = Either TLBNError

-- Catches error strings.
trapError :: Either TLBNError String -> Either TLBNError String
trapError = (flip CMonadEx.catchError) (return . show)

-- Extracts the value from a ThrowsError or errors out on error.
extractValue :: ThrowsError a -> a
extractValue (Left _) = error "Badness :-("
extractValue (Right val) = val

runThrows :: ThrowsError String -> String
runThrows action = extractValue $ trapError action

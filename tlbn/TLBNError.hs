module TLBNError where

-- Adapted from TAPL fullsimple error code.

import Control.Monad.Except
import Control.Monad.Error

data TLBNError = Default String
               
instance Show TLBNError where
    show (Default msg) = "Error: " ++ msg

instance Error TLBNError where
    noMsg  = Default "An unknown error has occurred."
    strMsg = Default

type ThrowsError = Either TLBNError

trapError :: Either TLBNError String -> Either TLBNError String
trapError = (flip catchError) (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runThrows :: ThrowsError String -> String
runThrows action = extractValue $ trapError action

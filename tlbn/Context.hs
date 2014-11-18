module Context where

-- Adapted from TAPL fullsimple context code.
-- Implements a simple context for the TLBN language.

import TLBNError
import TLBN
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

-- List of name, binding types.
newtype Context =
    Ctx [(String, Binding)]
    deriving Eq

-- Convenience type for dealing with errors and carrying state.
type ContextThrowsError = ErrorT TLBNError (State Context)

-- Creates a new Context
newContext :: Context
newContext = Ctx []

-- Returns the size of the context.
ctxLength :: Context -> Int
ctxLength (Ctx ps) = length ps

-- Appends a binding to a context.
appendBinding :: String -> Binding -> Context -> Context
appendBinding var binding (Ctx ps) = Ctx $ (var, binding) : ps

-- Returns the binding for a particular index in a context.
bindingOf :: Int -> Context -> ThrowsError Binding
bindingOf idx = (liftM snd) . (bindingPairOf idx)

-- Given an index and a context, returns the associated binding if one exists.
-- Otherwise, an error is thrown indicating that the variable isn't defined.
bindingPairOf :: Int -> Context -> ThrowsError (String, Binding)
bindingPairOf idx (Ctx ps) 
    = if idx >= length ps
      then throwError $ Default $ "Undefined variable at index " ++ show idx
      else return $ ps !! idx

liftThrows :: ThrowsError a -> ContextThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

-- Returns the variable ID of a particualr variable name. Throws an error if the
-- variable name is not found in the given context.
indexOf :: String -> Context -> ThrowsError Int
indexOf var (Ctx ps) = iter 0 ps
    where iter _ [] = throwError $ Default ("Undefined variable: " ++ var)
          iter i ((v,_):cs) | v == var  = return i
                            | otherwise = iter (i + 1) cs

runContextThrows :: ContextThrowsError a -> ThrowsError a
runContextThrows action = evalState (runErrorT action) newContext

-- Returns the name of a variable when given a variable ID.
nameOf :: Int -> Context -> ThrowsError String
nameOf idx = (liftM fst) . (bindingPairOf idx)

-- Helper routine primarily used for abstraction typing.
withBinding var b action = do
    ctx <- get
    put $ appendBinding var b ctx
    result <- action
    put ctx
    return result

-- Routine that allows the caller to temporarily use an old context.
withContext :: MonadState s m => s -> m b -> m b
withContext ctx action = do
    origCtx <- get
    put ctx
    result <- action
    put origCtx
    return result

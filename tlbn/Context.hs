module Context where

-- Adapted from TAPL fullsimple context code.

import TLBNError
import TLBN
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

newtype Context =
    Ctx [(String, Binding)]
    deriving Eq

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

bindingPairOf :: Int -> Context -> ThrowsError (String, Binding)
bindingPairOf idx (Ctx ps) 
    = if idx >= length ps
      then throwError $ Default $ "Undefined variable at index " ++ show idx
      else if idx < 0
           then throwError $ Default "Negative index for context"
           else return $ ps !! idx

liftThrows :: ThrowsError a -> ContextThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

indexOf :: String -> Context -> ThrowsError Int
indexOf var (Ctx ps) = iter 0 ps
    where iter _ [] = throwError $ Default ("Undefined variable: " ++ var)
          iter i ((v,_):cs) | v == var  = return i
                            | otherwise = iter (i + 1) cs

runContextThrows :: ContextThrowsError a -> ThrowsError a
runContextThrows action = evalState (runErrorT action) newContext

nameOf :: Int -> Context -> ThrowsError String
nameOf idx = (liftM fst) . (bindingPairOf idx)

withBinding var b action = do ctx <- get
                              put $ appendBinding var b ctx
                              result <- action
                              put ctx
                              return result

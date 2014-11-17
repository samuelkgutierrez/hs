module Context where

-- Adapted from TAPL fullsimple context code.

import TLBN
import Control.Monad.Except

newtype Context =
    Ctx [(String, Binding)]
    deriving (Show, Eq)

-- Creates a new Context
newContext :: Context
newContext = Ctx []

-- Appends a binding to a context.
appendBinding :: String -> Binding -> Context -> Context
appendBinding var binding (Ctx ps) = Ctx $ (var, binding) : ps

-- Returns the binding for a particular index in a context.
bindingOf :: Int -> Context -> Binding
bindingOf idx (Ctx ctx) = snd (ctx !! idx)

-- Returns the size of the context.
ctxLength :: Context -> Int
ctxLength (Ctx ps) = length ps

indexOf var (Ctx ps) = iter 0 ps
    where iter _ [] = throwError "Undefined variable."
          iter i ((v,_):cs) | v == var  = return i
                            | otherwise = iter (i + 1) cs

module Context where

-- Adapted from TAPL fullsimple context code.

import TLBN

newtype Context =
    Ctx [(String, Binding)]
    deriving (Show, Eq)

newContext :: Context
newContext = Ctx []

appendBinding :: String -> Binding -> Context -> Context
appendBinding var binding (Ctx ps) = Ctx $ (var,binding) : ps

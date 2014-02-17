--
-- Copyright (c) 2013 Samuel K. Gutierrez All rights reserved.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

-- a simple l-system implemented in haskell

-- cabal update
-- cabal install cabal-install
-- cabal install missingh
import Data.String.Utils
import Debug.Trace

--------------------------------------------------------------------------------
-- echoList
--------------------------------------------------------------------------------
echoBanner :: IO()
echoBanner = putStrLn banStr
    where banStr = "-- hsls: a simple l-system implemented in haskell"

--------------------------------------------------------------------------------
-- algae setup
--------------------------------------------------------------------------------
algaeSystem :: ([String], [String], [(String, String)])
algaeSystem = (v, w, p)
    where v = ["A", "B"]
          w = ["A"]
          p = [("A", "AB"), ("B", "A")] -- A --> AB, B --> A

--------------------------------------------------------------------------------
-- Cantor dust setup
--------------------------------------------------------------------------------
cantorDustSystem :: ([String], [String], [(String, String)])
cantorDustSystem = (v, w, p)
    where v = ["A", "B"]
          w = ["A"]
          p = [("A", "ABA"), ("B", "BBB")] -- A --> AB, B --> A

--------------------------------------------------------------------------------
-- rewrite
-- this function does all the heavy lifting. it takes the current state string
-- and applies the given production rules to it. the result is an updated state
-- string.
--------------------------------------------------------------------------------
rewrite :: [String] -> [(String, String)] -> [String]
rewrite [] productionRules = []
rewrite (s:sx) productionRules =
    let s' = snd (head (filter ((==s).fst) productionRules))
        -- at this point we have a String, so convert it to a list of single
        -- character Strings that represent the production
        sl = [[c] | c <- s']
        state' = sl ++ rewrite sx productionRules
    in state'

--------------------------------------------------------------------------------
-- lSystem
--------------------------------------------------------------------------------
lSystem :: [String] -> [String] -> [(String, String)] -> Int -> IO()
lSystem v w p ni
    | ni <= 0 = putStrLn "done!"
    | otherwise = do
      putStrLn (concat v)
      lSystem v' w p (ni - 1)
      where v' = rewrite v p

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------
main :: IO()
main = do
    echoBanner
    lSystem v w p ni -- alphabet, axiom, production rules, max iterations
    where (v, w, p) = algaeSystem
          ni = 10

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
-- rewrite
--------------------------------------------------------------------------------
rewrite :: [String] -> [(String, String)] -> [String]
rewrite [] productionRules = []
rewrite (s:sx) productionRules =
    let s' = snd (head (filter ((==s).fst) productionRules))
        foo = [[c] | c <- s']
        state' = foo ++ rewrite sx productionRules
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

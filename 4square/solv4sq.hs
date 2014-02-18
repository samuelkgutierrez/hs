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

--  The NPR Weekend Edition Sunday puzzle by Will Shortz, given 7 January 2007

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
    where banStr = "-- solv4sq: starting solve..."

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------
main :: IO()
main = do
    echoBanner

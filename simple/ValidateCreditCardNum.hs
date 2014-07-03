--
-- Copyright (c) 2014 Samuel K. Gutierrez All rights reserved.
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

-- ValidateCreditCardNum

import Data.List(transpose)

toDigits :: Integer -> [Integer]
toDigits number
    | number <= 0 = []
    | otherwise   = map read [[intStr] | intStr <- show number]

applyFToEvens :: (a -> a) -> [a] -> [a]
applyFToEvens _ [] = []
applyFToEvens f (i : is) =
    f i : applyFToEvens f is' where is' = drop 1 is

dropOdds :: [a] -> [a]
dropOdds [] = []
dropOdds (_ : as)
    | null as = []
    | otherwise = head as : dropOdds (drop 1 as)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l = concat $ transpose [evensLst, oddsLst]
    where evensLst = applyFToEvens (* 2) l
          oddsLst = dropOdds l

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits is = foldr (\i -> (+) (sum $ toDigits i)) 0 is

validate :: Integer -> Bool
validate cardNum = 0 == a
    where a = magic `mod` 10
          magic = sumDigits $ doubleEveryOther $ toDigits cardNum

main :: IO ()
main  = do
    let exampleCreditCardNum = 4012888888881881
    putStrLn $ "validating: " ++ show exampleCreditCardNum
    putStrLn $ "valid: " ++ show (validate exampleCreditCardNum)

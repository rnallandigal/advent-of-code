module AOC2023.Day01 (day01a, day01b) where

import Data.List.Split (splitOn)
import Data.List (find)
import Data.Char (isDigit, ord)
import Data.Maybe (fromJust)

import Debug.Trace (trace)

solve1 :: String -> Int
solve1 s = read [fromJust (find isDigit s), fromJust (find isDigit (reverse s))]

--solve2 :: String -> Int
--solve2 = 

day01a :: String -> String
day01a = show . sum . map solve1 . lines

day01b :: String -> String
day01b = const ""

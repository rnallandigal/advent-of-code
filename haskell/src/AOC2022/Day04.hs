module AOC2022.Day04 (day04a, day04b) where

import Data.List.Split (splitOn)

import Lib (between, count)

input :: String -> [[[Int]]]
input = map (map (map read . splitOn "-") . splitOn ",") . lines

day04a :: String -> String
day04a = show . count matches . input where
    matches [[a, b], [c, d]]
         = (c `between` (a, b) && d `between` (a, b))
        || (a `between` (c, d) && b `between` (c, d))

day04b :: String -> String
day04b = show . count matches . input where
    matches [[a, b], [c, d]]
        = a `between` (c, d) || c `between` (a, b)

module AOC2022.Day02 (day02a, day02b) where

import Data.Char (ord)

input :: String -> [(Int, Int)]
input = map (parse . words) . lines where
    parse [[a], [b]] = (ord a - ord 'A', ord b - ord 'X')

day02a :: String -> String
day02a = show . sum . map (\(a, b) -> 1 + b + 3 * ((b - a + 1) `mod` 3)) . input

day02b :: String -> String
day02b = show . sum . map (\(a, b) -> 1 + ((a + b - 1) `mod` 3) + 3 * b) . input

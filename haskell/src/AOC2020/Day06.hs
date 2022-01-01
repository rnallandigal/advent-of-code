module AOC2020.Day06 (day06a, day06b) where

import Data.List (intersect, union)
import Data.List.Split (splitOn)

day06a :: String -> String
day06a = show . sum . map (length . foldr1 union . lines) . splitOn "\n\n"

day06b :: String -> String
day06b = show . sum . map (length . foldr1 intersect . lines) . splitOn "\n\n"

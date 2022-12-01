module AOC2022.Day01 (day01a, day01b) where

import Data.List (sort)
import Data.List.Split (splitOn)

calorieCount :: String -> [Int]
calorieCount = map (sum . map read . lines) . splitOn "\n\n"

day01a :: String -> String
day01a = show . maximum . calorieCount

day01b :: String -> String
day01b = show . sum . take 3 . reverse . sort . calorieCount

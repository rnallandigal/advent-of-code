module AOC2020.Day01 (day01a, day01b) where

import Lib (ksum)

day01a :: String -> String
day01a = show . product . head . ksum 2 2020 . map read . lines

day01b :: String -> String
day01b = show . product . head . ksum 3 2020 . map read . lines

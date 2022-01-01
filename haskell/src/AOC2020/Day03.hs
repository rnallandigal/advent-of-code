module AOC2020.Day03 (day03a, day03b) where

import Lib (count)

slopes :: [(Int, Int)]
slopes = [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)]

trees :: (Int, Int) -> [String] -> Int
trees (dx, dy) as = count (=='#')
    [as !! y !! x | (x, y) <- zip [0,dx..] [0,dy..length as-1]]

day03a :: String -> String
day03a = show . trees (3, 1) . map cycle . lines

day03b :: String -> String
day03b = show . product . (\as -> map (`trees` as) slopes) . map cycle . lines

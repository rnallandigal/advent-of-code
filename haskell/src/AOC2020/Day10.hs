module AOC2020.Day10 (day10a, day10b) where

import Data.List (sort)

input :: String -> [Int]
input = (\xs -> (0:xs) ++ [last xs + 3]) . sort . map read . lines

day10a :: String -> String
day10a = show . (\(a, b, _) -> a * b) . foldl helper (0, 0, 0) . input where
    helper (a, b, x) y = case y - x of
        1 -> (a + 1, b, y)
        3 -> (a, b + 1, y)
        _ -> (a, b, y)

day10b :: String -> String
day10b = show . snd . head . foldl dp [] . input where
    dp acc 0 = (0, 1):acc
    dp acc k = (k, sum . map snd . takeWhile ((>=(k-3)) . fst) $ acc):acc

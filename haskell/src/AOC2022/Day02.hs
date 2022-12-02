module AOC2022.Day02 (day02a, day02b) where

import Data.Char (ord)
import Data.List.Split (splitOn)

input :: String -> [(Int, Int)]
input = map (parse . words) . lines where
    parse [[a], [b]] = (ord a - ord 'A', ord b - ord 'X')

score1 :: (Int, Int) -> Int
score1 (a, b) = 1 + b + win (a - b) where
    win (-2) = 0    -- rock vs scissors = lose
    win (-1) = 6    -- rock vs paper = win
    win ( 0) = 3    -- rock vs rock = draw
    win ( 1) = 0    -- paper vs rock = lose
    win ( 2) = 6    -- scissors vs rock = win

score2 :: (Int, Int) -> Int
score2 (a, b) = 1 + need a b + 3 * b where
    need a 0 = (a + 2) `mod` 3  -- need to lose, rock + 2 = scissors
    need a 1 = a                -- need to draw
    need a 2 = (a + 1) `mod` 3  -- need to win, rock + 1 = paper

day02a :: String -> String
day02a = show . sum . map score1 . input

day02b :: String -> String
day02b = show . sum . map score2 . input

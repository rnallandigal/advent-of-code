module AOC2022.Day03 (day03a, day03b) where

import           Data.Char (isAsciiLower, ord)
import           Data.List.Split (chunksOf, splitOn)
import qualified Data.Set as S

tr :: Char -> Int
tr c | isAsciiLower c = ord c - ord 'a' + 1
     | otherwise      = ord c - ord 'A' + 27

soln :: [[String]] -> Int
soln = sum . map (tr . head . S.toList . foldl1 S.intersection . map S.fromList)

day03a :: String -> String
day03a = show . soln
    . map (\x -> let n = length x `div` 2 in [take n x, drop n x])
    . lines

day03b :: String -> String
day03b = show . soln . chunksOf 3 . lines

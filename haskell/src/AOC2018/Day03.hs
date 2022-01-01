module AOC2018.Day03 (day03a, day03b) where

import           Data.Containers.ListUtils (nubOrd)
import           Data.List (null)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Text.Regex.TDFA

import           Lib (choose, count, freqMap)

lineRegex :: Regex
lineRegex = makeRegex "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"

input :: String -> [[Int]]
input = map (map read . tail) . match lineRegex

overlaps :: [[Int]] -> [(Int, Int)]
overlaps [[_, x1, y1, w1, h1], [_, x2, y2, w2, h2]] =
    let x3 = max x1 x2
        y3 = max y1 y2
        x4 = min (x1 + w1) (x2 + w2)
        y4 = min (y1 + h1) (y2 + h2)
    in  [(a, b) | a <- [x3..x4-1], b <- [y3..y4-1]]

day03a :: String -> String
day03a = show . length . nubOrd . concatMap overlaps . (`choose` 2) . input

day03b :: String -> String
day03b = show
    . map (head . fst)
    . filter (\(i, js) -> null $ concatMap (overlaps . (:[i])) js)
    . (\is -> map (\i -> (i, [j | j <- is, i /= j])) is)
    . input

-------------------------------------
-- Alternate solution
-------------------------------------

pts :: [Int] -> [(Int, Int)]
pts [_, x, y, w, h] = [(a, b) | a <- [x..x+w-1], b <- [y..y+h-1]]

day03a1 :: String -> String
day03a1 = show . count (>1) . M.elems . freqMap . concatMap pts . input

day03b1 :: String -> String
day03b1 = show
    . map head
    . (\(area, is) -> filter (all (==1) . mapMaybe (area M.!?) . pts) is)
    . (\is -> (freqMap . concatMap pts $ is, is))
    . input

module AOC2020.Day12 (day12a, day12b) where

import Data.List (foldl')

input :: String -> [(Char, Int)]
input = map (\(c:cs) -> (c, read cs)) . lines

rotate :: Int -> (Int, Int) -> (Int, Int)
rotate 0   (x, y) = ( x,  y)
rotate 90  (x, y) = (-y,  x)
rotate 180 (x, y) = (-x, -y)
rotate 270 (x, y) = ( y, -x)

dist :: (Int, Int) -> Int
dist (x, y) = abs x + abs y

day12a :: String -> String
day12a = show . dist . fst . foldl' step ((0, 0), (1, 0)) . input where
    step ((x, y), (dx, dy)) ('N', v) = ((x, y + v), (dx, dy))
    step ((x, y), (dx, dy)) ('S', v) = ((x, y - v), (dx, dy))
    step ((x, y), (dx, dy)) ('E', v) = ((x + v, y), (dx, dy))
    step ((x, y), (dx, dy)) ('W', v) = ((x - v, y), (dx, dy))
    step ((x, y), (dx, dy)) ('L', v) = ((x, y), rotate v (dx, dy))
    step ((x, y), (dx, dy)) ('R', v) = ((x, y), rotate (360 - v) (dx, dy))
    step ((x, y), (dx, dy)) ('F', v) = ((x + dx * v, y + dy * v), (dx, dy))

day12b :: String -> String
day12b = show . dist . fst . foldl' step ((0, 0), (10, 1)) . input where
    step ((x, y), (dx, dy)) ('N', v) = ((x, y), (dx, dy + v))
    step ((x, y), (dx, dy)) ('S', v) = ((x, y), (dx, dy - v))
    step ((x, y), (dx, dy)) ('E', v) = ((x, y), (dx + v, dy))
    step ((x, y), (dx, dy)) ('W', v) = ((x, y), (dx - v, dy))
    step ((x, y), (dx, dy)) ('L', v) = ((x, y), rotate v (dx, dy))
    step ((x, y), (dx, dy)) ('R', v) = ((x, y), rotate (360 - v) (dx, dy))
    step ((x, y), (dx, dy)) ('F', v) = ((x + dx * v, y + dy * v), (dx, dy))

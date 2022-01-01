module AOC2020.Day13 (day13a, day13b) where

import Data.Bifunctor
import Data.Function (on)
import Data.List (find, minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

input :: String -> (Int, [(Int, Int)])
input = bimap read buses . (\[x, y] -> (x, y)) . lines where
    buses = map (second read) . filter ((/="x") . snd) . zip [0..] . splitOn ","

day13a :: String -> String
day13a = show . foldr1 (*) . (\(t, xs) -> minimumBy (lag t) xs) . input where
    lag t = compare `on` (\(_, d) -> d * ((t + d - 1) `div` d) - t)

-- t s.t. x | (t + i) and y | (t + j)
offsetLCM :: (Int, Int) -> (Int, Int) -> (Int, Int)
offsetLCM (i, x) (j, y) = (offset, lcm x y) where
    offset = fromJust $ find (\t -> (t + j) `mod` y == 0) [i,i+x..]

day13b :: String -> String
day13b = show . fst . foldl1 offsetLCM . snd . input

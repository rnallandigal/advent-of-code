module AOC2020.Day02 (day02a, day02b) where

import Data.List.Split (splitOneOf)
import Lib (between, count)

input :: String -> [(Int, Int, Char, String)]
input = map ((\[w,x,y,_,z] -> (read w, read x, head y, z)) . splitOneOf "- :")
    . lines

day02a :: String -> String
day02a = show . count valid . input where
    valid (l, h, c, s) = count (==c) s `between` (l, h)

day02b :: String -> String
day02b = show . count valid . input where
    valid (i, j, c, s) = foldr1 (/=) $ map ((==c) . (s !!)) [i - 1, j - 1]

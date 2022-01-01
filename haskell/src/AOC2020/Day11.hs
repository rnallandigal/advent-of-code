module AOC2020.Day11 (day11a, day11b) where

import qualified Data.HashMap.Strict as M
import           Data.Maybe (mapMaybe)

import           Lib (count, fixedPoint)

type Seats = M.HashMap (Int, Int) Char

input :: String -> Seats
input = M.fromList
    . concat
    . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [1..]) [1..]
    . lines

step :: Int -> Char -> [Char] -> Char
step _ '.' _  = '.'
step _ 'L' xs = if '#' `notElem` xs then '#' else 'L'
step t '#' xs = if count (=='#') xs >= t then 'L' else '#'

find :: (a -> Bool) -> [Maybe a] -> Maybe a
find f (Nothing:_) = Nothing
find f (Just x:xs) = if f x then Just x else find f xs

slopes = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

adjacent :: (Char -> Bool) -> Seats -> (Int, Int) -> [Char]
adjacent f m (x, y) = mapMaybe (find f . map (m M.!?) . dir) slopes where
    dir (dx, dy) = tail $ iterate (\(i, j) -> (i + dx, j + dy)) (x, y)

run :: (Char -> Bool) -> Int -> Seats -> Seats
run f t m = M.mapWithKey (\k v -> step t v $ adjacent f m k) m

day11a :: String -> String
day11a = show . count (=='#') . M.elems . fixedPoint (run (const True) 4) . input

day11b :: String -> String
day11b = show . count (=='#') . M.elems . fixedPoint (run (/='.') 5) . input

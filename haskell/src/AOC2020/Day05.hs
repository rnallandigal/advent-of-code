module AOC2020.Day05 (day05a, day05b) where

seatId :: String -> Int
seatId = foldl (\a x -> a * 2 + if x == 'B' || x == 'R' then 1 else 0) 0

day05a :: String -> String
day05a = show . maximum . map seatId . lines

day05b :: String -> String
day05b = show . missing . map seatId . lines where
    missing xs = (length xs + 1) * (minimum xs + maximum xs) `div` 2 - sum xs

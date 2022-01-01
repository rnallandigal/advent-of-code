module AOC2018.Day01 (day01a, day01b) where

import qualified Data.Set as S

import           Lib (firstDuplicate, readInt)

day01a :: String -> String
day01a = show . sum . map readInt . lines

day01b :: String -> String
day01b = show . firstDuplicate . scanl (+) 0 . cycle . map readInt . lines

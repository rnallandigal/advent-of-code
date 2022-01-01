module AOC2018.Day02 (day02a, day02b) where

import           Control.Monad (guard)
import           Data.Function (on)
import           Data.List (group, intersect, maximumBy, sort)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)

import           Lib (choose, freqMap)

day02a :: String -> String
day02a = show
    . product . map length . group . sort
    . concatMap (intersect [2, 3] . M.elems . freqMap)
    . lines

day02b :: String -> String
day02b = maximumBy (compare `on` length)
    . map (\[x, y] -> catMaybes $ zipWith (\c d -> guard (c == d) *> Just c) x y)
    . (`choose` 2)
    . lines

module AOC2020.Day09 (day09a, day09b) where

import           Data.List (find)
import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Vector as V

import           Lib (choose, ksum)

input :: String -> V.Vector Int
input = V.fromList . map read . lines

ifind :: (Int -> a -> Bool) -> V.Vector a -> Maybe a
ifind f = fmap fst . V.find snd . V.imap (\i x -> (x, f i x))

firstNot2Sum :: V.Vector Int -> Maybe Int
firstNot2Sum vs = ifind (\i t -> null . ksum 2 t . window $ i) $ V.drop 25 vs
    where window i = V.toList $ V.slice i 25 vs

day09a :: String -> String
day09a = show . fromJust . firstNot2Sum . input

day09b :: String -> String
day09b s = show $ (\ws -> V.minimum ws + V.maximum ws) weakness where
    nums        = input s
    len         = length nums
    target      = fromJust . firstNot2Sum $ nums
    subarrays   = [V.slice i l nums | l <- [2..len], i <- [0..len-l]]
    weakness    = fromJust $ find (\ws -> V.sum ws == target) subarrays

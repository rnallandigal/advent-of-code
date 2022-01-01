module AOC2020.Day14 (day14a, day14b) where

import qualified Data.IntMap as M
import           Data.List (foldl', unfoldr)
import           Data.List.Split (splitOn)
import           Data.Tuple (swap)

import           Lib (outcomes)

type Mask = String
type Mem  = M.IntMap Int
data Inst = SetMask Mask | Store Int Int

input :: String -> [Inst]
input = map (inst . splitOn " = ") . lines where
    inst ["mask", m] = SetMask $ reverse m
    inst [k, v]      = Store (read . init . drop 4 $ k) (read v)

bin :: Int -> [Int]
bin = take 36 . unfoldr (Just . swap . (`divMod` 2))

dec :: [Int] -> Int
dec = sum . zipWith (*) (iterate (*2) 1)

run :: (Mask -> Int -> Int -> [(Int, Int)]) -> (Mem, Mask) -> Inst -> (Mem, Mask)
run _ (ram, mask) (SetMask m) = (ram, m)
run f (ram, mask) (Store i j) = (foldr (uncurry M.insert) ram $ f mask i j, mask)

decoderV1 :: Mask -> Int -> Int -> [(Int, Int)]
decoderV1 m k v = [(k, dec $ zipWith tr (bin v) m)] where
    tr i 'X' = i
    tr _ '0' = 0
    tr _ '1' = 1

decoderV2 :: Mask -> Int -> Int -> [(Int, Int)]
decoderV2 m k v = map (\x -> (dec x, v)) $ outcomes tr (zip (bin k) m) where
    tr (_, 'X') = [0, 1]
    tr (i, '0') = [i]
    tr (_, '1') = [1]

day14a :: String -> String
day14a = show . sum . fst . foldl' (run decoderV1) (M.empty, []) . input

day14b :: String -> String
day14b = show . sum . fst . foldl' (run decoderV2) (M.empty, []) . input

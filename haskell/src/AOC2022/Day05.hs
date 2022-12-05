module AOC2022.Day05 (day05a, day05b) where

import           Data.Bifunctor (bimap, second)
import           Data.List (foldl', sortOn)
import           Data.List.Split (splitOn)
import qualified Data.Vector as V
import           Linear.V2

import           Lib (indexed, pair)

type Crates = V.Vector (V.Vector Char)
type Inst   = (Int, Int, Int)

input :: String -> (Crates, [Inst])
input = bimap crates insts . pair . splitOn "\n\n" where
    crates s = V.fromList $ map (vec . filt) indices where
        grid    = indexed . init $ lines s
        indices = [1,5..(length . head $ lines s)]
        filt i  = filter (\((V2 _ x), c) -> x == i && c /= ' ') grid
        vec     = V.fromList . map snd . sortOn (\((V2 y _), _) -> -y)
    insts                   = map (inst . words) . lines
    inst [_, n, _, i, _, j] = (read n, read i - 1, read j - 1)

step :: (V.Vector Char -> V.Vector Char) -> Crates -> Inst -> Crates
step f crates (n, i, j) = crates V.// [(i, prefix), (j, dst V.++ suffix)] where
    src              = crates `V.unsafeIndex` i
    dst              = crates `V.unsafeIndex` j
    (prefix, suffix) = second f $ V.splitAt (V.length src - n) src

day05a :: String -> String
day05a = V.toList . V.map V.last . uncurry (foldl' (step V.reverse)) . input

day05b :: String -> String
day05b = V.toList . V.map V.last . uncurry (foldl' (step id)) . input

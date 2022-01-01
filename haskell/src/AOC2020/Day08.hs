module AOC2020.Day08 (day08a, day08b) where

import           Data.Bifunctor
import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Vector as V

import           Lib (readInt)

input :: String -> V.Vector (String, Int)
input = V.fromList . map ((\[a, b] -> (a, readInt b)) . splitOn " ") . lines

type Inst = (Int, Int) -> Int -> (Int, Int)
inst "nop" = \(ip, acc) arg -> (ip + 1, acc)
inst "acc" = \(ip, acc) arg -> (ip + 1, acc + arg)
inst "jmp" = \(ip, acc) arg -> (ip + arg, acc)

run :: V.Vector (Inst, Int) -> (Bool, Int)
run prog = helper S.empty (0, 0) where
    helper seen state@(ip, acc)
        | ip == V.length prog = (True, acc)
        | ip >  V.length prog = (False, acc)
        | ip `S.member` seen  = (False, acc)
        | otherwise           = let (inst, arg) = prog V.! ip
                                in  helper (S.insert ip seen) (inst state arg)

day08a :: String -> String
day08a = show . snd . run . V.map (first inst) . input

flipInst :: String -> String
flipInst "nop" = "jmp"
flipInst "jmp" = "nop"
flipInst "acc" = "acc"

day08b :: String -> String
day08b = show . snd . fromJust
    . V.find fst
    . V.map (run . V.map (first inst))
    . (\vs -> V.imap (\i v -> vs V.// [(i, first flipInst v)]) vs)
    . input

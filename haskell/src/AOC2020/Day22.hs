module AOC2020.Day22 (day22a, day22b) where

import           Data.Foldable (toList)
import           Data.List.Split (splitOn)
import           Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import           Lib (pair)

type Deck   = Seq Int
data Player = P1 | P2

input :: String -> (Deck, Deck)
input = pair . map (Seq.fromList . map read . tail . lines) . splitOn "\n\n"

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

day22a :: String -> String
day22a = show . score . combat . input where
    combat (p1, Empty) = p1
    combat (Empty, p2) = p2
    combat (x :<| xs, y :<| ys)
        | x > y     = combat (xs |> x |> y, ys)
        | otherwise = combat (xs, ys |> y |> x)

day22b :: String -> String
day22b = show . score . snd . combat S.empty . input where
    combat _ (p1, Empty) = (P1, p1)
    combat _ (Empty, p2) = (P2, p2)
    combat seen game@(x :<| xs, y :<| ys)
        | game `S.member` seen = (P1, x <| xs)
        | otherwise            = combat (S.insert game seen) $
                                 case winner game of
                                     P1 -> (xs |> x |> y, ys)
                                     P2 -> (xs, ys |> y |> x)

    winner game@(x :<| xs, y :<| ys)
        | x > Seq.length xs || y > Seq.length ys = if x > y then P1 else P2
        | otherwise = let xs' = Seq.take x xs
                          ys' = Seq.take y ys
                      in  if maximum xs' > maximum ys'
                          then P1   -- only P1 is guaranteed to win in this case
                          else fst $ combat S.empty (xs', ys')

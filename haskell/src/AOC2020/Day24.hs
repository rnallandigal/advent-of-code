module AOC2020.Day24 (day24a, day24b) where

import           Data.Either (fromRight)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Linear.V3
import           Text.Parsec hiding (count)
import           Text.Parsec.Char

import           Lib (count)

input :: String -> [[V3 Int]]
input = map (fromRight [] . parse (many dir) "") . lines

neighbors :: V.Vector (V3 Int)
neighbors = V.fromList [ (V3 1 (-1) 0), (V3 0 (-1) 1), (V3 (-1) 0 1)
                       , (V3 (-1) 1 0), (V3 0 1 (-1)), (V3 1 0 (-1)) ]

dir :: Parsec String () (V3 Int)
dir =     (char 'e'     >> pure (neighbors V.! 0))
  <|> try (string "se"  >> pure (neighbors V.! 1))
  <|>     (string "sw"  >> pure (neighbors V.! 2))
  <|>     (char 'w'     >> pure (neighbors V.! 3))
  <|> try (string "nw"  >> pure (neighbors V.! 4))
  <|>     (string "ne"  >> pure (neighbors V.! 5))

setup :: String -> S.Set (V3 Int)
setup = foldr switch S.empty . map sum . input where
    switch p s = if p `S.member` s then S.delete p s else S.insert p s

step :: S.Set (V3 Int) -> S.Set (V3 Int)
step tiles = foldr rules S.empty candidates where
    candidates = foldr (\p s -> foldr S.insert s $ neighbors' p) S.empty tiles
    neighbors' p = V.map (+p) neighbors
    rules p s | blacks == 2 || (black && blacks == 1) = S.insert p s
              | otherwise                             = s
              where blacks = count (`S.member` tiles) (neighbors' p)
                    black  = p `S.member` tiles

day24a :: String -> String
day24a = show . S.size . setup

day24b :: String -> String
day24b = show . S.size . (!! 100) . iterate step . setup

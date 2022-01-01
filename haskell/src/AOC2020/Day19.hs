module AOC2020.Day19 (day19a, day19b) where

import           Data.Bifunctor
import           Data.List (foldl1')
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           Text.ParserCombinators.ReadP hiding (count)

import           Lib (count, pair)

type Grammar = M.Map Int [[Token]]
data Token = NonTerminal Int | Terminal Char deriving (Show)

input :: String -> (Grammar, [String])
input = first grammar . pair . map lines . splitOn "\n\n" where
    grammar = M.fromList . map rule
    rule = bimap read (map prod . splitOn " | ") . pair . splitOn ": "
    prod ['"', c, '"'] = [Terminal c]
    prod xs            = map (NonTerminal . read) $ splitOn " " xs

build :: Grammar -> Token -> ReadP ()
build g (Terminal c)    = char c >> pure ()
build g (NonTerminal i) = choice $ map (foldl1' (>>) . map (build g)) (g M.! i)

solve :: ReadP a -> [String] -> Int
solve p xs = count (\i -> (not $ null i) && (null . snd $ last i)) res where
    res = map (readP_to_S p) xs

day19a :: String -> String
day19a = show . uncurry solve . first (flip build (NonTerminal 0)) . input

day19b :: String -> String
day19b s = show $ solve (build g' (NonTerminal 0)) xs where
    g'      = foldr (uncurry M.insert) g rules
    rules   = map (second (map (map NonTerminal)))
              [ ( 8, [ [42], [42, 8] ] ), ( 11, [[42, 31], [42, 11, 31]] ) ]
    (g, xs) = input s

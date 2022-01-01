module AOC2020.Day07 (day07a, day07b) where

import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

import           Lib (count, fixedPoint)

interpret :: [String] -> (String, [(Int, String)])
interpret [x, "no other bags."] = (x, [])
interpret [x, y] = (x, map (bags . words) $ splitOn ", " y)
    where bags (w:x:y:_) = (read w, x ++ " " ++ y)

input :: String -> M.Map String [(Int, String)]
input = M.fromList . map (interpret . splitOn " bags contain ") . lines

bfs :: M.Map String [(Int, String)] -> M.Map String Bool -> M.Map String Bool
bfs m xs = M.mapWithKey helper xs where
    helper _ True = True
    helper b _    = any (\(_, c) -> xs M.! c) $ m M.! b

day07a :: String -> String
day07a = show
    . count (==True)
    . M.elems
    . (\m -> fixedPoint (bfs m) $ M.map (any $ (=="shiny gold") . snd) m)
    . input

day07b :: String -> String
day07b = show . (\m -> dfs m "shiny gold" - 1) . input where
    dfs m b = 1 + foldr (\(i, b') acc -> dfs m b' * i + acc) 0 (m M.! b)

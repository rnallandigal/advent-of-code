module AOC2020.Day21 (day21a, day21b) where

import           Data.List (intercalate, sortOn)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Lib (count, fixedPoint)

input :: String -> [(S.Set String, S.Set String)]
input = map (helper . splitOn " (contains " . init) . lines where
    helper [x, y] = (S.fromList $ words x, S.fromList $ splitOn ", " y)

collectFacts :: [(S.Set String, S.Set String)] -> M.Map String (S.Set String)
collectFacts foods = M.fromSet (`common` foods) allergies where
    allergies = S.unions $ map snd foods
    common a = foldr1 S.intersection . map fst . filter ((a `S.member`) . snd)

step :: M.Map String (S.Set String) -> M.Map String String -> M.Map String String
step facts soln = M.foldrWithKey check soln facts where
    check a is soln =
        let js = S.filter (`M.notMember` soln) is
        in  if S.size js == 1 then M.insert (S.findMin js) a soln else soln

solve :: [(S.Set String, S.Set String)] -> M.Map String String
solve foods = fixedPoint (step $ collectFacts foods) M.empty

day21a :: String -> String
day21a s = show $ count (`M.notMember` soln) ingredients where
    foods = input s
    soln = solve foods
    ingredients = concatMap (S.toList . fst) foods

day21b :: String -> String
day21b = intercalate "," . map fst . sortOn snd . M.toList . solve . input

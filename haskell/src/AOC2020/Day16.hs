module AOC2020.Day16 (day16a, day16b) where

import           Data.Bifunctor
import qualified Data.IntSet as IS
import           Data.List (isPrefixOf, sortOn, transpose)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Tuple (swap)

import           Lib (assign, between, pShow, pair)

type Rule   = [(Int, Int)]
type Rules  = [(String, Rule)]
type Ticket = [Int]

input :: String -> (Rules, [Ticket], IS.IntSet)
input = addRange . parse . map lines . splitOn "\n\n" where
    addRange (rs, ts) = (rs, ts, IS.fromList $ validRange rs)
    parse [a, b, c]   = (map rule a, map ticket $ last b : tail c)
    ticket            = map read . splitOn ","
    rule              = second (map cond . splitOn " or ") . pair . splitOn ": "
    cond              = pair . map read . splitOn "-"
    validRange        = concatMap (\(l, h) -> [l..h]) . concat . map snd

day16a :: String -> String
day16a = show . sum . map sum . pickInvalid . input where
    pickInvalid (_, ts, range) = map (filter (`IS.notMember` range)) ts

day16b :: String -> String
day16b s = show . product $ map fst departures where
    departures      = filter (("departure" `isPrefixOf`) . snd) ticket
    ticket          = zip t . map fst . sortOn snd . M.toList $ matching
    matching        = head . assign $ map (second (S.fromList . candidates)) rs
    candidates rule = map fst $ filter (all (check rule) . snd) fields
    check rule i    = any (i `between`) rule
    fields          = zip [0..] $ transpose validTs
    validTs@(t:_)   = filter (null . filter (`IS.notMember` range)) ts
    (rs, ts, range) = input s

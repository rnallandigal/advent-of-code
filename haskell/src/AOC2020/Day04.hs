module AOC2020.Day04 (day04a, day04b) where

import           Data.Either (isRight)
import           Data.Functor.Identity
import           Data.List.Split (chunksOf, endByOneOf, splitOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Text.Parsec hiding (between, count)
import qualified Text.Parsec as P (count)

import           Lib (between, count, pairs)

type Parser = ParsecT String () Identity

input :: String -> [M.Map String String]
input = map (M.fromList . pairs . endByOneOf ": \n") . splitOn "\n\n"

satisfies :: Parser a -> String -> Bool
satisfies p s = isRight $ parse (p <* eof) "" s

validateHeight :: String -> Bool
validateHeight h = validate $ splitAt (length h - 2) h
    where   validate (x, "cm") = read x `between` (150, 193)
            validate (x, "in") = read x `between` (59, 76)
            validate _         = False

eyeColors :: [String]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

fields :: [(String, String -> Bool)]
fields =
    [ ("byr", \x -> read x `between` (1920, 2002))
    , ("iyr", \x -> read x `between` (2010, 2020))
    , ("eyr", \x -> read x `between` (2020, 2030))
    , ("hgt", validateHeight)
    , ("hcl", satisfies $ char '#' <* P.count 6 hexDigit)
    , ("ecl", satisfies (choice $ map (try . string) eyeColors))
    , ("pid", satisfies $ P.count 9 digit)
    ]

day04a :: String -> String
day04a = show . count (\m -> all ((`M.member` m) . fst) fields) . input

day04b :: String -> String
day04b = show . count valid . input
    where valid m = all (fromMaybe False . (\(k, f) -> f <$> (m M.!? k))) fields

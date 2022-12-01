module Main where

import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import           Text.Printf (printf)

import           Command

-- file path must match module name
-- module names may not start with a digit
import qualified AOC2018.Day01 as AOC2018
import qualified AOC2018.Day02 as AOC2018
import qualified AOC2018.Day03 as AOC2018
import qualified AOC2018.Day04 as AOC2018

import qualified AOC2020.Day01 as AOC2020
import qualified AOC2020.Day02 as AOC2020
import qualified AOC2020.Day03 as AOC2020
import qualified AOC2020.Day04 as AOC2020
import qualified AOC2020.Day05 as AOC2020
import qualified AOC2020.Day06 as AOC2020
import qualified AOC2020.Day07 as AOC2020
import qualified AOC2020.Day08 as AOC2020
import qualified AOC2020.Day09 as AOC2020
import qualified AOC2020.Day10 as AOC2020
import qualified AOC2020.Day11 as AOC2020
import qualified AOC2020.Day12 as AOC2020
import qualified AOC2020.Day13 as AOC2020
import qualified AOC2020.Day14 as AOC2020
import qualified AOC2020.Day15 as AOC2020
import qualified AOC2020.Day16 as AOC2020
import qualified AOC2020.Day17 as AOC2020
import qualified AOC2020.Day18 as AOC2020
import qualified AOC2020.Day19 as AOC2020
import qualified AOC2020.Day20 as AOC2020
import qualified AOC2020.Day21 as AOC2020
import qualified AOC2020.Day22 as AOC2020
import qualified AOC2020.Day23 as AOC2020
import qualified AOC2020.Day24 as AOC2020
import qualified AOC2020.Day25 as AOC2020

import qualified AOC2022.Day01 as AOC2022

problems :: M.Map (Int, Int, Int) (String -> String)
problems = M.fromList
    [ ((2018,  1, 1), AOC2018.day01a)
    , ((2018,  1, 2), AOC2018.day01b)
    , ((2018,  2, 1), AOC2018.day02a)
    , ((2018,  2, 2), AOC2018.day02b)
    , ((2018,  3, 1), AOC2018.day03a)
    , ((2018,  3, 2), AOC2018.day03b)
    , ((2018,  4, 1), AOC2018.day04a)
    , ((2018,  4, 2), AOC2018.day04b)

    , ((2020,  1, 1), AOC2020.day01a)
    , ((2020,  1, 2), AOC2020.day01b)
    , ((2020,  2, 1), AOC2020.day02a)
    , ((2020,  2, 2), AOC2020.day02b)
    , ((2020,  3, 1), AOC2020.day03a)
    , ((2020,  3, 2), AOC2020.day03b)
    , ((2020,  4, 1), AOC2020.day04a)
    , ((2020,  4, 2), AOC2020.day04b)
    , ((2020,  5, 1), AOC2020.day05a)
    , ((2020,  5, 2), AOC2020.day05b)
    , ((2020,  6, 1), AOC2020.day06a)
    , ((2020,  6, 2), AOC2020.day06b)
    , ((2020,  7, 1), AOC2020.day07a)
    , ((2020,  7, 2), AOC2020.day07b)
    , ((2020,  8, 1), AOC2020.day08a)
    , ((2020,  8, 2), AOC2020.day08b)
    , ((2020,  9, 1), AOC2020.day09a)
    , ((2020,  9, 2), AOC2020.day09b)
    , ((2020, 10, 1), AOC2020.day10a)
    , ((2020, 10, 2), AOC2020.day10b)
    , ((2020, 11, 1), AOC2020.day11a)
    , ((2020, 11, 2), AOC2020.day11b)
    , ((2020, 12, 1), AOC2020.day12a)
    , ((2020, 12, 2), AOC2020.day12b)
    , ((2020, 13, 1), AOC2020.day13a)
    , ((2020, 13, 2), AOC2020.day13b)
    , ((2020, 14, 1), AOC2020.day14a)
    , ((2020, 14, 2), AOC2020.day14b)
    , ((2020, 15, 1), AOC2020.day15a)
    , ((2020, 15, 2), AOC2020.day15b)
    , ((2020, 16, 1), AOC2020.day16a)
    , ((2020, 16, 2), AOC2020.day16b)
    , ((2020, 17, 1), AOC2020.day17a)
    , ((2020, 17, 2), AOC2020.day17b)
    , ((2020, 18, 1), AOC2020.day18a)
    , ((2020, 18, 2), AOC2020.day18b)
    , ((2020, 19, 1), AOC2020.day19a)
    , ((2020, 19, 2), AOC2020.day19b)
    , ((2020, 20, 1), AOC2020.day20a)
    , ((2020, 20, 2), AOC2020.day20b)
    , ((2020, 21, 1), AOC2020.day21a)
    , ((2020, 21, 2), AOC2020.day21b)
    , ((2020, 22, 1), AOC2020.day22a)
    , ((2020, 22, 2), AOC2020.day22b)
    , ((2020, 23, 1), AOC2020.day23a)
    , ((2020, 23, 2), AOC2020.day23b)
    , ((2020, 24, 1), AOC2020.day24a)
    , ((2020, 24, 2), AOC2020.day24b)
    , ((2020, 25, 1), AOC2020.day25a)

    , ((2022,  1, 1), AOC2022.day01a)
    , ((2022,  1, 2), AOC2022.day01b) ]

main :: IO ()
main = do
    cmd <- readCommand
    results <- getResults cmd
    mapM_ printSoln results

getResults :: Command -> IO [((Int, Int, Int), String)]
getResults (Problem p) =
    case M.lookupGE p problems of
        Nothing -> error "cannot find solution for the given problem"
        Just f  -> runSoln f <&> \s -> [(p, s)]
getResults All = zip (M.keys problems) <$> mapM runSoln (M.toList problems)

printSoln :: ((Int, Int, Int), String) -> IO ()
printSoln ((y, d, p), s) = putStrLn $ printf "%04d.%02d part %d: %s" y d p s

runSoln :: ((Int, Int, Int), String -> String) -> IO String
runSoln ((y, d, _), f) = readFile (printf "in/%04d.%02d.in" y d) <&> f

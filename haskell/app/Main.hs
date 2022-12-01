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

import qualified AOC2021.Day01 as AOC2021
import qualified AOC2021.Day02 as AOC2021
import qualified AOC2021.Day03 as AOC2021
import qualified AOC2021.Day04 as AOC2021
import qualified AOC2021.Day05 as AOC2021
import qualified AOC2021.Day06 as AOC2021
import qualified AOC2021.Day07 as AOC2021
import qualified AOC2021.Day08 as AOC2021
import qualified AOC2021.Day09 as AOC2021
import qualified AOC2021.Day10 as AOC2021
import qualified AOC2021.Day11 as AOC2021
import qualified AOC2021.Day12 as AOC2021
import qualified AOC2021.Day13 as AOC2021
import qualified AOC2021.Day14 as AOC2021
import qualified AOC2021.Day15 as AOC2021
import qualified AOC2021.Day16 as AOC2021
import qualified AOC2021.Day17 as AOC2021
import qualified AOC2021.Day18 as AOC2021
import qualified AOC2021.Day19 as AOC2021
import qualified AOC2021.Day20 as AOC2021
import qualified AOC2021.Day21 as AOC2021
import qualified AOC2021.Day22 as AOC2021
import qualified AOC2021.Day23 as AOC2021
import qualified AOC2021.Day24 as AOC2021
import qualified AOC2021.Day25 as AOC2021

import qualified AOC2022.Day01 as AOC2022
import qualified AOC2022.Day02 as AOC2022
import qualified AOC2022.Day03 as AOC2022
import qualified AOC2022.Day04 as AOC2022
import qualified AOC2022.Day05 as AOC2022
import qualified AOC2022.Day06 as AOC2022
import qualified AOC2022.Day07 as AOC2022
import qualified AOC2022.Day08 as AOC2022
import qualified AOC2022.Day09 as AOC2022
import qualified AOC2022.Day10 as AOC2022
import qualified AOC2022.Day11 as AOC2022
import qualified AOC2022.Day12 as AOC2022
import qualified AOC2022.Day13 as AOC2022
import qualified AOC2022.Day14 as AOC2022
import qualified AOC2022.Day15 as AOC2022
import qualified AOC2022.Day16 as AOC2022
import qualified AOC2022.Day17 as AOC2022
import qualified AOC2022.Day18 as AOC2022
import qualified AOC2022.Day19 as AOC2022
import qualified AOC2022.Day20 as AOC2022
import qualified AOC2022.Day21 as AOC2022
import qualified AOC2022.Day22 as AOC2022
import qualified AOC2022.Day23 as AOC2022
import qualified AOC2022.Day24 as AOC2022
import qualified AOC2022.Day25 as AOC2022

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

    , ((2021,  1, 1), AOC2021.day01a)
    , ((2021,  1, 2), AOC2021.day01b)
    , ((2021,  2, 1), AOC2021.day02a)
    , ((2021,  2, 2), AOC2021.day02b)
    , ((2021,  3, 1), AOC2021.day03a)
    , ((2021,  3, 2), AOC2021.day03b)
    , ((2021,  4, 1), AOC2021.day04a)
    , ((2021,  4, 2), AOC2021.day04b)
    , ((2021,  5, 1), AOC2021.day05a)
    , ((2021,  5, 2), AOC2021.day05b)
    , ((2021,  6, 1), AOC2021.day06a)
    , ((2021,  6, 2), AOC2021.day06b)
    , ((2021,  7, 1), AOC2021.day07a)
    , ((2021,  7, 2), AOC2021.day07b)
    , ((2021,  8, 1), AOC2021.day08a)
    , ((2021,  8, 2), AOC2021.day08b)
    , ((2021,  9, 1), AOC2021.day09a)
    , ((2021,  9, 2), AOC2021.day09b)
    , ((2021, 10, 1), AOC2021.day10a)
    , ((2021, 10, 2), AOC2021.day10b)
    , ((2021, 11, 1), AOC2021.day11a)
    , ((2021, 11, 2), AOC2021.day11b)
    , ((2021, 12, 1), AOC2021.day12a)
    , ((2021, 12, 2), AOC2021.day12b)
    , ((2021, 13, 1), AOC2021.day13a)
    , ((2021, 13, 2), AOC2021.day13b)
    , ((2021, 14, 1), AOC2021.day14a)
    , ((2021, 14, 2), AOC2021.day14b)
    , ((2021, 15, 1), AOC2021.day15a)
    , ((2021, 15, 2), AOC2021.day15b)
    , ((2021, 16, 1), AOC2021.day16a)
    , ((2021, 16, 2), AOC2021.day16b)
    , ((2021, 17, 1), AOC2021.day17a)
    , ((2021, 17, 2), AOC2021.day17b)
    , ((2021, 18, 1), AOC2021.day18a)
    , ((2021, 18, 2), AOC2021.day18b)
    , ((2021, 19, 1), AOC2021.day19a)
    , ((2021, 19, 2), AOC2021.day19b)
    , ((2021, 20, 1), AOC2021.day20a)
    , ((2021, 20, 2), AOC2021.day20b)
    , ((2021, 21, 1), AOC2021.day21a)
    , ((2021, 21, 2), AOC2021.day21b)
    , ((2021, 22, 1), AOC2021.day22a)
    , ((2021, 22, 2), AOC2021.day22b)
    , ((2021, 23, 1), AOC2021.day23a)
    , ((2021, 23, 2), AOC2021.day23b)
    , ((2021, 24, 1), AOC2021.day24a)
    , ((2021, 24, 2), AOC2021.day24b)
    , ((2021, 25, 1), AOC2021.day25a)

    , ((2022,  1, 1), AOC2022.day01a)
    , ((2022,  1, 2), AOC2022.day01b)
    , ((2022,  2, 1), AOC2022.day02a)
    , ((2022,  2, 2), AOC2022.day02b)
    , ((2022,  3, 1), AOC2022.day03a)
    , ((2022,  3, 2), AOC2022.day03b)
    , ((2022,  4, 1), AOC2022.day04a)
    , ((2022,  4, 2), AOC2022.day04b)
    , ((2022,  5, 1), AOC2022.day05a)
    , ((2022,  5, 2), AOC2022.day05b)
    , ((2022,  6, 1), AOC2022.day06a)
    , ((2022,  6, 2), AOC2022.day06b)
    , ((2022,  7, 1), AOC2022.day07a)
    , ((2022,  7, 2), AOC2022.day07b)
    , ((2022,  8, 1), AOC2022.day08a)
    , ((2022,  8, 2), AOC2022.day08b)
    , ((2022,  9, 1), AOC2022.day09a)
    , ((2022,  9, 2), AOC2022.day09b)
    , ((2022, 10, 1), AOC2022.day10a)
    , ((2022, 10, 2), AOC2022.day10b)
    , ((2022, 11, 1), AOC2022.day11a)
    , ((2022, 11, 2), AOC2022.day11b)
    , ((2022, 12, 1), AOC2022.day12a)
    , ((2022, 12, 2), AOC2022.day12b)
    , ((2022, 13, 1), AOC2022.day13a)
    , ((2022, 13, 2), AOC2022.day13b)
    , ((2022, 14, 1), AOC2022.day14a)
    , ((2022, 14, 2), AOC2022.day14b)
    , ((2022, 15, 1), AOC2022.day15a)
    , ((2022, 15, 2), AOC2022.day15b)
    , ((2022, 16, 1), AOC2022.day16a)
    , ((2022, 16, 2), AOC2022.day16b)
    , ((2022, 17, 1), AOC2022.day17a)
    , ((2022, 17, 2), AOC2022.day17b)
    , ((2022, 18, 1), AOC2022.day18a)
    , ((2022, 18, 2), AOC2022.day18b)
    , ((2022, 19, 1), AOC2022.day19a)
    , ((2022, 19, 2), AOC2022.day19b)
    , ((2022, 20, 1), AOC2022.day20a)
    , ((2022, 20, 2), AOC2022.day20b)
    , ((2022, 21, 1), AOC2022.day21a)
    , ((2022, 21, 2), AOC2022.day21b)
    , ((2022, 22, 1), AOC2022.day22a)
    , ((2022, 22, 2), AOC2022.day22b)
    , ((2022, 23, 1), AOC2022.day23a)
    , ((2022, 23, 2), AOC2022.day23b)
    , ((2022, 24, 1), AOC2022.day24a)
    , ((2022, 24, 2), AOC2022.day24b)
    , ((2022, 25, 1), AOC2022.day25a) ]

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

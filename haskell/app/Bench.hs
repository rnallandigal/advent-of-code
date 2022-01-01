module Main where

import           Criterion
import           Criterion.Main

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

main = defaultMain
    [ env (readFile "in/2018.01.in") (bench "2018.01.1" . nf AOC2018.day01a)
    , env (readFile "in/2018.01.in") (bench "2018.01.2" . nf AOC2018.day01b)
    , env (readFile "in/2018.02.in") (bench "2018.02.1" . nf AOC2018.day02a)
    , env (readFile "in/2018.02.in") (bench "2018.02.2" . nf AOC2018.day02b)
    , env (readFile "in/2018.03.in") (bench "2018.03.1" . nf AOC2018.day03a)
    , env (readFile "in/2018.03.in") (bench "2018.03.2" . nf AOC2018.day03b)
    , env (readFile "in/2018.04.in") (bench "2018.04.1" . nf AOC2018.day04a)
    , env (readFile "in/2018.04.in") (bench "2018.04.2" . nf AOC2018.day04b)
   
    , env (readFile "in/2020.01.in") (bench "2020.01.1" . nf AOC2020.day01a)
    , env (readFile "in/2020.01.in") (bench "2020.01.2" . nf AOC2020.day01b)
    , env (readFile "in/2020.02.in") (bench "2020.02.1" . nf AOC2020.day02a)
    , env (readFile "in/2020.02.in") (bench "2020.02.2" . nf AOC2020.day02b)
    , env (readFile "in/2020.03.in") (bench "2020.03.1" . nf AOC2020.day03a)
    , env (readFile "in/2020.03.in") (bench "2020.03.2" . nf AOC2020.day03b)
    , env (readFile "in/2020.04.in") (bench "2020.04.1" . nf AOC2020.day04a)
    , env (readFile "in/2020.04.in") (bench "2020.04.2" . nf AOC2020.day04b)
    , env (readFile "in/2020.05.in") (bench "2020.05.1" . nf AOC2020.day05a)
    , env (readFile "in/2020.05.in") (bench "2020.05.2" . nf AOC2020.day05b)
    , env (readFile "in/2020.06.in") (bench "2020.06.1" . nf AOC2020.day06a)
    , env (readFile "in/2020.06.in") (bench "2020.06.2" . nf AOC2020.day06b)
    , env (readFile "in/2020.07.in") (bench "2020.07.1" . nf AOC2020.day07a)
    , env (readFile "in/2020.07.in") (bench "2020.07.2" . nf AOC2020.day07b)
    , env (readFile "in/2020.08.in") (bench "2020.08.1" . nf AOC2020.day08a)
    , env (readFile "in/2020.08.in") (bench "2020.08.2" . nf AOC2020.day08b)
    , env (readFile "in/2020.09.in") (bench "2020.09.1" . nf AOC2020.day09a)
    , env (readFile "in/2020.09.in") (bench "2020.09.2" . nf AOC2020.day09b)
    , env (readFile "in/2020.10.in") (bench "2020.10.1" . nf AOC2020.day10a)
    , env (readFile "in/2020.10.in") (bench "2020.10.2" . nf AOC2020.day10b)
    , env (readFile "in/2020.11.in") (bench "2020.11.1" . nf AOC2020.day11a)
    , env (readFile "in/2020.11.in") (bench "2020.11.2" . nf AOC2020.day11b)
    , env (readFile "in/2020.12.in") (bench "2020.12.1" . nf AOC2020.day12a)
    , env (readFile "in/2020.12.in") (bench "2020.12.2" . nf AOC2020.day12b)
    , env (readFile "in/2020.13.in") (bench "2020.13.1" . nf AOC2020.day13a)
    , env (readFile "in/2020.13.in") (bench "2020.13.2" . nf AOC2020.day13b)
    , env (readFile "in/2020.14.in") (bench "2020.14.1" . nf AOC2020.day14a)
    , env (readFile "in/2020.14.in") (bench "2020.14.2" . nf AOC2020.day14b)
    , env (readFile "in/2020.15.in") (bench "2020.15.1" . nf AOC2020.day15a)
    , env (readFile "in/2020.15.in") (bench "2020.15.2" . nf AOC2020.day15b)
    , env (readFile "in/2020.16.in") (bench "2020.16.1" . nf AOC2020.day16a)
    , env (readFile "in/2020.16.in") (bench "2020.16.2" . nf AOC2020.day16b)
    , env (readFile "in/2020.17.in") (bench "2020.17.1" . nf AOC2020.day17a)
    , env (readFile "in/2020.17.in") (bench "2020.17.2" . nf AOC2020.day17b)
    , env (readFile "in/2020.18.in") (bench "2020.18.1" . nf AOC2020.day18a)
    , env (readFile "in/2020.18.in") (bench "2020.18.2" . nf AOC2020.day18b)
    , env (readFile "in/2020.19.in") (bench "2020.19.1" . nf AOC2020.day19a)
    , env (readFile "in/2020.19.in") (bench "2020.19.2" . nf AOC2020.day19b)
    , env (readFile "in/2020.20.in") (bench "2020.20.1" . nf AOC2020.day20a)
    , env (readFile "in/2020.20.in") (bench "2020.20.2" . nf AOC2020.day20b)
    , env (readFile "in/2020.21.in") (bench "2020.21.1" . nf AOC2020.day21a)
    , env (readFile "in/2020.21.in") (bench "2020.21.2" . nf AOC2020.day21b)
    , env (readFile "in/2020.22.in") (bench "2020.22.1" . nf AOC2020.day22a)
    , env (readFile "in/2020.22.in") (bench "2020.22.2" . nf AOC2020.day22b)
    , env (readFile "in/2020.23.in") (bench "2020.23.1" . nf AOC2020.day23a)
    , env (readFile "in/2020.23.in") (bench "2020.23.2" . nf AOC2020.day23b)
    , env (readFile "in/2020.24.in") (bench "2020.24.1" . nf AOC2020.day24a)
    , env (readFile "in/2020.24.in") (bench "2020.24.2" . nf AOC2020.day24b)
    , env (readFile "in/2020.25.in") (bench "2020.25.1" . nf AOC2020.day25a) ]

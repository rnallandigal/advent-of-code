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
    , env (readFile "in/2020.25.in") (bench "2020.25.1" . nf AOC2020.day25a)

    , env (readFile "in/2021.01.in") (bench "2021.01.1" . nf AOC2021.day01a)
    , env (readFile "in/2021.01.in") (bench "2021.01.2" . nf AOC2021.day01b)
    , env (readFile "in/2021.02.in") (bench "2021.02.1" . nf AOC2021.day02a)
    , env (readFile "in/2021.02.in") (bench "2021.02.2" . nf AOC2021.day02b)
    , env (readFile "in/2021.03.in") (bench "2021.03.1" . nf AOC2021.day03a)
    , env (readFile "in/2021.03.in") (bench "2021.03.2" . nf AOC2021.day03b)
    , env (readFile "in/2021.04.in") (bench "2021.04.1" . nf AOC2021.day04a)
    , env (readFile "in/2021.04.in") (bench "2021.04.2" . nf AOC2021.day04b)
    , env (readFile "in/2021.05.in") (bench "2021.05.1" . nf AOC2021.day05a)
    , env (readFile "in/2021.05.in") (bench "2021.05.2" . nf AOC2021.day05b)
    , env (readFile "in/2021.06.in") (bench "2021.06.1" . nf AOC2021.day06a)
    , env (readFile "in/2021.06.in") (bench "2021.06.2" . nf AOC2021.day06b)
    , env (readFile "in/2021.07.in") (bench "2021.07.1" . nf AOC2021.day07a)
    , env (readFile "in/2021.07.in") (bench "2021.07.2" . nf AOC2021.day07b)
    , env (readFile "in/2021.08.in") (bench "2021.08.1" . nf AOC2021.day08a)
    , env (readFile "in/2021.08.in") (bench "2021.08.2" . nf AOC2021.day08b)
    , env (readFile "in/2021.09.in") (bench "2021.09.1" . nf AOC2021.day09a)
    , env (readFile "in/2021.09.in") (bench "2021.09.2" . nf AOC2021.day09b)
    , env (readFile "in/2021.10.in") (bench "2021.10.1" . nf AOC2021.day10a)
    , env (readFile "in/2021.10.in") (bench "2021.10.2" . nf AOC2021.day10b)
    , env (readFile "in/2021.11.in") (bench "2021.11.1" . nf AOC2021.day11a)
    , env (readFile "in/2021.11.in") (bench "2021.11.2" . nf AOC2021.day11b)
    , env (readFile "in/2021.12.in") (bench "2021.12.1" . nf AOC2021.day12a)
    , env (readFile "in/2021.12.in") (bench "2021.12.2" . nf AOC2021.day12b)
    , env (readFile "in/2021.13.in") (bench "2021.13.1" . nf AOC2021.day13a)
    , env (readFile "in/2021.13.in") (bench "2021.13.2" . nf AOC2021.day13b)
    , env (readFile "in/2021.14.in") (bench "2021.14.1" . nf AOC2021.day14a)
    , env (readFile "in/2021.14.in") (bench "2021.14.2" . nf AOC2021.day14b)
    , env (readFile "in/2021.15.in") (bench "2021.15.1" . nf AOC2021.day15a)
    , env (readFile "in/2021.15.in") (bench "2021.15.2" . nf AOC2021.day15b)
    , env (readFile "in/2021.16.in") (bench "2021.16.1" . nf AOC2021.day16a)
    , env (readFile "in/2021.16.in") (bench "2021.16.2" . nf AOC2021.day16b)
    , env (readFile "in/2021.17.in") (bench "2021.17.1" . nf AOC2021.day17a)
    , env (readFile "in/2021.17.in") (bench "2021.17.2" . nf AOC2021.day17b)
    , env (readFile "in/2021.18.in") (bench "2021.18.1" . nf AOC2021.day18a)
    , env (readFile "in/2021.18.in") (bench "2021.18.2" . nf AOC2021.day18b)
    , env (readFile "in/2021.19.in") (bench "2021.19.1" . nf AOC2021.day19a)
    , env (readFile "in/2021.19.in") (bench "2021.19.2" . nf AOC2021.day19b)
    , env (readFile "in/2021.20.in") (bench "2021.20.1" . nf AOC2021.day20a)
    , env (readFile "in/2021.20.in") (bench "2021.20.2" . nf AOC2021.day20b)
    , env (readFile "in/2021.21.in") (bench "2021.21.1" . nf AOC2021.day21a)
    , env (readFile "in/2021.21.in") (bench "2021.21.2" . nf AOC2021.day21b)
    , env (readFile "in/2021.22.in") (bench "2021.22.1" . nf AOC2021.day22a)
    , env (readFile "in/2021.22.in") (bench "2021.22.2" . nf AOC2021.day22b)
    , env (readFile "in/2021.23.in") (bench "2021.23.1" . nf AOC2021.day23a)
    , env (readFile "in/2021.23.in") (bench "2021.23.2" . nf AOC2021.day23b)
    , env (readFile "in/2021.24.in") (bench "2021.24.1" . nf AOC2021.day24a)
    , env (readFile "in/2021.24.in") (bench "2021.24.2" . nf AOC2021.day24b)
    , env (readFile "in/2021.25.in") (bench "2021.25.1" . nf AOC2021.day25a)

    , env (readFile "in/2022.01.in") (bench "2022.01.1" . nf AOC2022.day01a)
    , env (readFile "in/2022.01.in") (bench "2022.01.2" . nf AOC2022.day01b)
    , env (readFile "in/2022.02.in") (bench "2022.02.1" . nf AOC2022.day02a)
    , env (readFile "in/2022.02.in") (bench "2022.02.2" . nf AOC2022.day02b)
    , env (readFile "in/2022.03.in") (bench "2022.03.1" . nf AOC2022.day03a)
    , env (readFile "in/2022.03.in") (bench "2022.03.2" . nf AOC2022.day03b)
    , env (readFile "in/2022.04.in") (bench "2022.04.1" . nf AOC2022.day04a)
    , env (readFile "in/2022.04.in") (bench "2022.04.2" . nf AOC2022.day04b)
    , env (readFile "in/2022.05.in") (bench "2022.05.1" . nf AOC2022.day05a)
    , env (readFile "in/2022.05.in") (bench "2022.05.2" . nf AOC2022.day05b)
    , env (readFile "in/2022.06.in") (bench "2022.06.1" . nf AOC2022.day06a)
    , env (readFile "in/2022.06.in") (bench "2022.06.2" . nf AOC2022.day06b)
    , env (readFile "in/2022.07.in") (bench "2022.07.1" . nf AOC2022.day07a)
    , env (readFile "in/2022.07.in") (bench "2022.07.2" . nf AOC2022.day07b)
    , env (readFile "in/2022.08.in") (bench "2022.08.1" . nf AOC2022.day08a)
    , env (readFile "in/2022.08.in") (bench "2022.08.2" . nf AOC2022.day08b)
    , env (readFile "in/2022.09.in") (bench "2022.09.1" . nf AOC2022.day09a)
    , env (readFile "in/2022.09.in") (bench "2022.09.2" . nf AOC2022.day09b)
    , env (readFile "in/2022.10.in") (bench "2022.10.1" . nf AOC2022.day10a)
    , env (readFile "in/2022.10.in") (bench "2022.10.2" . nf AOC2022.day10b)
    , env (readFile "in/2022.11.in") (bench "2022.11.1" . nf AOC2022.day11a)
    , env (readFile "in/2022.11.in") (bench "2022.11.2" . nf AOC2022.day11b)
    , env (readFile "in/2022.12.in") (bench "2022.12.1" . nf AOC2022.day12a)
    , env (readFile "in/2022.12.in") (bench "2022.12.2" . nf AOC2022.day12b)
    , env (readFile "in/2022.13.in") (bench "2022.13.1" . nf AOC2022.day13a)
    , env (readFile "in/2022.13.in") (bench "2022.13.2" . nf AOC2022.day13b)
    , env (readFile "in/2022.14.in") (bench "2022.14.1" . nf AOC2022.day14a)
    , env (readFile "in/2022.14.in") (bench "2022.14.2" . nf AOC2022.day14b)
    , env (readFile "in/2022.15.in") (bench "2022.15.1" . nf AOC2022.day15a)
    , env (readFile "in/2022.15.in") (bench "2022.15.2" . nf AOC2022.day15b)
    , env (readFile "in/2022.16.in") (bench "2022.16.1" . nf AOC2022.day16a)
    , env (readFile "in/2022.16.in") (bench "2022.16.2" . nf AOC2022.day16b)
    , env (readFile "in/2022.17.in") (bench "2022.17.1" . nf AOC2022.day17a)
    , env (readFile "in/2022.17.in") (bench "2022.17.2" . nf AOC2022.day17b)
    , env (readFile "in/2022.18.in") (bench "2022.18.1" . nf AOC2022.day18a)
    , env (readFile "in/2022.18.in") (bench "2022.18.2" . nf AOC2022.day18b)
    , env (readFile "in/2022.19.in") (bench "2022.19.1" . nf AOC2022.day19a)
    , env (readFile "in/2022.19.in") (bench "2022.19.2" . nf AOC2022.day19b)
    , env (readFile "in/2022.20.in") (bench "2022.20.1" . nf AOC2022.day20a)
    , env (readFile "in/2022.20.in") (bench "2022.20.2" . nf AOC2022.day20b)
    , env (readFile "in/2022.21.in") (bench "2022.21.1" . nf AOC2022.day21a)
    , env (readFile "in/2022.21.in") (bench "2022.21.2" . nf AOC2022.day21b)
    , env (readFile "in/2022.22.in") (bench "2022.22.1" . nf AOC2022.day22a)
    , env (readFile "in/2022.22.in") (bench "2022.22.2" . nf AOC2022.day22b)
    , env (readFile "in/2022.23.in") (bench "2022.23.1" . nf AOC2022.day23a)
    , env (readFile "in/2022.23.in") (bench "2022.23.2" . nf AOC2022.day23b)
    , env (readFile "in/2022.24.in") (bench "2022.24.1" . nf AOC2022.day24a)
    , env (readFile "in/2022.24.in") (bench "2022.24.2" . nf AOC2022.day24b)
    , env (readFile "in/2022.25.in") (bench "2022.25.1" . nf AOC2022.day25a) ]

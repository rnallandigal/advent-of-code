# Advent of Code
https://adventofcode.com

## Installation
### C++
1. Clone the repository
2. Install dependencies
    1. gcc (C++20 capable)
    2. fmt: https://fmt.dev/latest/index.html
    3. docopt: http://docopt.org/
    4. Google benchmark: https://github.com/google/benchmark
    5. pthread (See your distribution's documentation on installing the pthread library)
3. `(mkdir .build && cd .build && cmake .. && make)`
4. `.build/aoc --help`
5. `.build/bench --help`

## Benchmarks
CPU: Intel i7 4800MQ

| Language | Year | Total Time (s) |
|:--------:|:----:|:--------------:|
|    C++   | 2020 |      1.6       |

### C++
```
2021-11-08T16:38:03+05:30
Running .build/bench
Run on (12 X 4100 MHz CPU s)
CPU Caches:
  L1 Data 32 KiB (x6)
  L1 Instruction 32 KiB (x6)
  L2 Unified 256 KiB (x6)
  L3 Unified 9216 KiB (x1)
Load Average: 0.35, 0.34, 0.26
--------------------------------------------------------
Benchmark              Time             CPU   Iterations
--------------------------------------------------------
AOC_2018_01_1      0.032 ms        0.032 ms        22017
AOC_2018_01_2       9.29 ms         9.21 ms           75
AOC_2018_02_1      0.037 ms        0.037 ms        18477
AOC_2018_02_2      0.077 ms        0.077 ms         9446
AOC_2018_03_1       5.33 ms         5.31 ms          132
AOC_2018_03_2       5.05 ms         5.03 ms          155
AOC_2018_04_1       1.42 ms         1.41 ms          500
AOC_2018_04_2       1.40 ms         1.40 ms          501
AOC_2020_01_1      0.019 ms        0.019 ms        35744
AOC_2020_01_2      0.075 ms        0.075 ms         9430
AOC_2020_02_1      0.527 ms        0.525 ms         1347
AOC_2020_02_2      0.514 ms        0.513 ms         1376
AOC_2020_03_1      0.015 ms        0.015 ms        46457
AOC_2020_03_2      0.017 ms        0.017 ms        41280
AOC_2020_04_1      0.650 ms        0.650 ms         1048
AOC_2020_04_2      0.792 ms        0.791 ms          891
AOC_2020_05_1      0.020 ms        0.020 ms        35672
AOC_2020_05_2      0.030 ms        0.030 ms        23509
AOC_2020_06_1      0.175 ms        0.175 ms         3985
AOC_2020_06_2      0.176 ms        0.176 ms         4013
AOC_2020_07_1       2.62 ms         2.62 ms          270
AOC_2020_07_2       2.28 ms         2.28 ms          309
AOC_2020_08_1      0.034 ms        0.034 ms        20731
AOC_2020_08_2      0.443 ms        0.443 ms         1585
AOC_2020_09_1      0.113 ms        0.113 ms         6175
AOC_2020_09_2      0.114 ms        0.114 ms         6159
AOC_2020_10_1      0.004 ms        0.004 ms       179592
AOC_2020_10_2      0.004 ms        0.004 ms       185444
AOC_2020_11_1       24.8 ms         24.8 ms           28
AOC_2020_11_2       38.9 ms         38.8 ms           18
AOC_2020_12_1      0.028 ms        0.027 ms        25778
AOC_2020_12_2      0.027 ms        0.027 ms        25544
AOC_2020_13_1      0.002 ms        0.002 ms       461082
AOC_2020_13_2      0.003 ms        0.003 ms       212113
AOC_2020_14_1      0.192 ms        0.192 ms         3646
AOC_2020_14_2       8.39 ms         8.38 ms           84
AOC_2020_15_1      0.006 ms        0.006 ms       127372
AOC_2020_15_2        432 ms          432 ms            2
AOC_2020_16_1      0.396 ms        0.395 ms         1788
AOC_2020_16_2      0.502 ms        0.501 ms         1403
AOC_2020_17_1      0.642 ms        0.641 ms         1101
AOC_2020_17_2       18.0 ms         17.9 ms           39
AOC_2020_18_1      0.131 ms        0.130 ms         5384
AOC_2020_18_2      0.131 ms        0.131 ms         5322
AOC_2020_19_1       25.1 ms         25.1 ms           28
AOC_2020_19_2       52.7 ms         52.6 ms           13
AOC_2020_20_1      0.372 ms        0.371 ms         1899
AOC_2020_20_2       1.68 ms         1.68 ms          413
AOC_2020_21_1      0.457 ms        0.456 ms         1528
AOC_2020_21_2      0.715 ms        0.713 ms          964
AOC_2020_22_1      0.006 ms        0.006 ms       112999
AOC_2020_22_2        567 ms          566 ms            1
AOC_2020_23_1      0.001 ms        0.001 ms       838403
AOC_2020_23_2        211 ms          210 ms            3
AOC_2020_24_1      0.338 ms        0.337 ms         2074
AOC_2020_24_2        174 ms          174 ms            4
AOC_2020_25_1      0.519 ms        0.518 ms         1349
```

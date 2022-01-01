# Advent of Code
https://adventofcode.com

## Acknowledgements
Thanks to Justin Le (https://github.com/mstksg/advent-of-code-2020) for inspiration on haskell solutions.

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

### Haskell
1. Clone the repository
2. Install Haskell stack (https://docs.haskellstack.org/en/stable/README/)
3. `stack build`
4. `stack exec aoc -- --help`
5. `stack exec bench -- --help`

## Benchmarks
CPU: Intel i7 8750H

| Language | Year | Total Time (s) |
|:--------:|:----:|:--------------:|
|    C++   | 2020 |      1.6       |
|  Haskell | 2020 |      5.0       |

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

### Haskell
```
benchmarking 2020.01.1
time                 944.1 μs   (893.1 μs .. 1.016 ms)
                     0.978 R²   (0.966 R² .. 0.995 R²)
mean                 911.2 μs   (887.3 μs .. 957.6 μs)
std dev              115.5 μs   (74.62 μs .. 181.3 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking 2020.01.2
time                 46.61 ms   (45.91 ms .. 47.16 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 46.03 ms   (45.65 ms .. 46.37 ms)
std dev              714.2 μs   (511.5 μs .. 960.2 μs)

benchmarking 2020.02.1
time                 5.687 ms   (5.643 ms .. 5.724 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.661 ms   (5.637 ms .. 5.692 ms)
std dev              82.20 μs   (58.00 μs .. 113.1 μs)

benchmarking 2020.02.2
time                 5.851 ms   (5.786 ms .. 5.914 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 5.794 ms   (5.752 ms .. 5.848 ms)
std dev              147.4 μs   (114.6 μs .. 224.7 μs)

benchmarking 2020.03.1
time                 1.594 ms   (1.582 ms .. 1.602 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.586 ms   (1.574 ms .. 1.607 ms)
std dev              51.08 μs   (28.28 μs .. 85.12 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking 2020.03.2
time                 3.512 ms   (3.379 ms .. 3.707 ms)
                     0.983 R²   (0.969 R² .. 0.999 R²)
mean                 3.433 ms   (3.385 ms .. 3.599 ms)
std dev              230.0 μs   (148.0 μs .. 375.5 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking 2020.04.1
time                 4.234 ms   (4.195 ms .. 4.275 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 4.213 ms   (4.182 ms .. 4.242 ms)
std dev              95.31 μs   (71.66 μs .. 129.7 μs)

benchmarking 2020.04.2
time                 6.997 ms   (6.917 ms .. 7.090 ms)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 7.100 ms   (7.038 ms .. 7.257 ms)
std dev              277.6 μs   (112.0 μs .. 502.7 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 2020.05.1
time                 353.8 μs   (350.0 μs .. 359.4 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 360.8 μs   (356.0 μs .. 370.4 μs)
std dev              20.21 μs   (11.46 μs .. 36.70 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking 2020.05.2
time                 848.1 μs   (840.7 μs .. 856.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 846.5 μs   (839.9 μs .. 857.1 μs)
std dev              31.44 μs   (21.10 μs .. 48.28 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking 2020.06.1
time                 2.838 ms   (2.809 ms .. 2.869 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.853 ms   (2.840 ms .. 2.879 ms)
std dev              67.06 μs   (40.36 μs .. 100.4 μs)

benchmarking 2020.06.2
time                 2.528 ms   (2.510 ms .. 2.548 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.551 ms   (2.534 ms .. 2.577 ms)
std dev              63.34 μs   (33.55 μs .. 88.29 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking 2020.07.1
time                 17.08 ms   (16.91 ms .. 17.21 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 17.15 ms   (17.06 ms .. 17.25 ms)
std dev              238.4 μs   (166.3 μs .. 370.0 μs)

benchmarking 2020.07.2
time                 7.506 ms   (7.430 ms .. 7.571 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 7.482 ms   (7.434 ms .. 7.542 ms)
std dev              149.5 μs   (100.9 μs .. 224.8 μs)

benchmarking 2020.08.1
time                 936.6 μs   (926.5 μs .. 949.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 936.9 μs   (931.4 μs .. 948.8 μs)
std dev              26.31 μs   (17.51 μs .. 41.11 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking 2020.08.2
time                 22.84 ms   (22.67 ms .. 23.03 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 22.96 ms   (22.86 ms .. 23.14 ms)
std dev              307.7 μs   (167.6 μs .. 502.3 μs)

benchmarking 2020.09.1
time                 3.430 ms   (3.398 ms .. 3.470 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 3.431 ms   (3.409 ms .. 3.484 ms)
std dev              110.0 μs   (76.40 μs .. 173.4 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 2020.09.2
time                 4.870 ms   (4.829 ms .. 4.915 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.914 ms   (4.889 ms .. 4.939 ms)
std dev              91.36 μs   (75.25 μs .. 118.5 μs)

benchmarking 2020.10.1
time                 155.9 μs   (155.1 μs .. 156.9 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 156.2 μs   (155.3 μs .. 158.1 μs)
std dev              3.883 μs   (2.007 μs .. 6.495 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 2020.10.2
time                 157.7 μs   (156.8 μs .. 158.6 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 158.4 μs   (157.6 μs .. 159.6 μs)
std dev              3.496 μs   (2.502 μs .. 4.719 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 2020.11.1
time                 546.4 ms   (528.4 ms .. 559.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 542.2 ms   (537.2 ms .. 545.3 ms)
std dev              4.875 ms   (2.326 ms .. 6.219 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 2020.11.2
time                 777.7 ms   (751.4 ms .. 814.2 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 766.1 ms   (760.7 ms .. 771.5 ms)
std dev              7.051 ms   (3.141 ms .. 9.289 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 2020.12.1
time                 922.7 μs   (912.3 μs .. 941.5 μs)
                     0.993 R²   (0.985 R² .. 0.997 R²)
mean                 955.0 μs   (937.0 μs .. 980.4 μs)
std dev              76.68 μs   (50.55 μs .. 101.3 μs)
variance introduced by outliers: 64% (severely inflated)

benchmarking 2020.12.2
time                 1.055 ms   (1.026 ms .. 1.088 ms)
                     0.978 R²   (0.943 R² .. 0.998 R²)
mean                 1.054 ms   (1.027 ms .. 1.103 ms)
std dev              122.7 μs   (31.61 μs .. 207.4 μs)
variance introduced by outliers: 79% (severely inflated)

benchmarking 2020.13.1
time                 33.00 μs   (32.48 μs .. 33.90 μs)
                     0.993 R²   (0.988 R² .. 0.997 R²)
mean                 34.35 μs   (33.67 μs .. 35.76 μs)
std dev              3.254 μs   (2.660 μs .. 4.053 μs)
variance introduced by outliers: 83% (severely inflated)

benchmarking 2020.13.2
time                 37.88 μs   (36.61 μs .. 39.21 μs)
                     0.993 R²   (0.988 R² .. 0.998 R²)
mean                 36.14 μs   (35.47 μs .. 37.01 μs)
std dev              2.498 μs   (1.850 μs .. 3.552 μs)
variance introduced by outliers: 71% (severely inflated)

benchmarking 2020.14.1
time                 4.308 ms   (4.068 ms .. 4.615 ms)
                     0.946 R²   (0.892 R² .. 0.991 R²)
mean                 4.251 ms   (4.104 ms .. 4.464 ms)
std dev              578.4 μs   (347.9 μs .. 965.7 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking 2020.14.2
time                 62.70 ms   (52.90 ms .. 73.90 ms)
                     0.958 R²   (0.889 R² .. 0.992 R²)
mean                 66.15 ms   (62.62 ms .. 72.17 ms)
std dev              7.746 ms   (4.516 ms .. 11.21 ms)
variance introduced by outliers: 43% (moderately inflated)

benchmarking 2020.15.1
time                 11.64 μs   (11.04 μs .. 12.32 μs)
                     0.986 R²   (0.979 R² .. 0.995 R²)
mean                 11.87 μs   (11.53 μs .. 12.33 μs)
std dev              1.334 μs   (1.064 μs .. 1.605 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking 2020.15.2
time                 363.0 ms   (320.7 ms .. 399.0 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 405.1 ms   (385.9 ms .. 440.3 ms)
std dev              34.39 ms   (2.597 ms .. 42.16 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking 2020.16.1
time                 14.41 ms   (13.79 ms .. 15.15 ms)
                     0.986 R²   (0.978 R² .. 0.993 R²)
mean                 13.05 ms   (12.73 ms .. 13.52 ms)
std dev              977.1 μs   (785.2 μs .. 1.207 ms)
variance introduced by outliers: 37% (moderately inflated)

benchmarking 2020.16.2
time                 13.09 ms   (12.67 ms .. 13.55 ms)
                     0.991 R²   (0.984 R² .. 0.995 R²)
mean                 12.92 ms   (12.63 ms .. 13.20 ms)
std dev              772.0 μs   (589.3 μs .. 979.9 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking 2020.17.1
time                 11.86 ms   (10.37 ms .. 12.96 ms)
                     0.847 R²   (0.623 R² .. 0.996 R²)
mean                 13.23 ms   (12.30 ms .. 15.66 ms)
std dev              3.258 ms   (423.0 μs .. 5.337 ms)
variance introduced by outliers: 88% (severely inflated)

benchmarking 2020.17.2
time                 298.8 ms   (291.0 ms .. 306.2 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 319.0 ms   (309.9 ms .. 335.8 ms)
std dev              16.80 ms   (3.716 ms .. 23.24 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 2020.18.1
time                 8.812 ms   (8.540 ms .. 9.184 ms)
                     0.987 R²   (0.971 R² .. 0.997 R²)
mean                 9.161 ms   (9.030 ms .. 9.329 ms)
std dev              454.2 μs   (321.9 μs .. 645.5 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking 2020.18.2
time                 9.237 ms   (9.017 ms .. 9.488 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 9.380 ms   (9.261 ms .. 9.514 ms)
std dev              349.5 μs   (278.7 μs .. 490.4 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking 2020.19.1
time                 8.350 ms   (7.496 ms .. 10.20 ms)
                     0.701 R²   (0.516 R² .. 0.995 R²)
mean                 7.570 ms   (7.045 ms .. 9.242 ms)
std dev              2.596 ms   (428.4 μs .. 5.397 ms)
variance introduced by outliers: 94% (severely inflated)

benchmarking 2020.19.2
time                 31.90 ms   (24.22 ms .. 35.05 ms)
                     0.875 R²   (0.661 R² .. 0.992 R²)
mean                 38.13 ms   (35.34 ms .. 44.48 ms)
std dev              8.749 ms   (2.322 ms .. 14.27 ms)
variance introduced by outliers: 79% (severely inflated)

benchmarking 2020.20.1
time                 9.648 ms   (8.862 ms .. 10.08 ms)
                     0.975 R²   (0.961 R² .. 0.989 R²)
mean                 9.267 ms   (8.953 ms .. 9.620 ms)
std dev              932.7 μs   (767.6 μs .. 1.153 ms)
variance introduced by outliers: 56% (severely inflated)

benchmarking 2020.20.2
time                 131.3 ms   (114.6 ms .. 143.7 ms)
                     0.982 R²   (0.950 R² .. 0.998 R²)
mean                 133.2 ms   (127.9 ms .. 139.6 ms)
std dev              9.377 ms   (6.537 ms .. 13.19 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking 2020.21.1
time                 5.597 ms   (5.518 ms .. 5.679 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 5.840 ms   (5.755 ms .. 5.997 ms)
std dev              353.2 μs   (260.1 μs .. 451.2 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking 2020.21.2
time                 5.165 ms   (5.109 ms .. 5.209 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 5.179 ms   (5.138 ms .. 5.219 ms)
std dev              124.5 μs   (84.30 μs .. 172.5 μs)

benchmarking 2020.22.1
time                 123.3 μs   (120.7 μs .. 127.8 μs)
                     0.995 R²   (0.989 R² .. 1.000 R²)
mean                 122.1 μs   (121.2 μs .. 124.7 μs)
std dev              5.074 μs   (2.860 μs .. 9.100 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking 2020.22.2
time                 4.195 ms   (4.152 ms .. 4.245 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 4.252 ms   (4.201 ms .. 4.340 ms)
std dev              211.8 μs   (77.63 μs .. 355.9 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking 2020.23.1
time                 16.17 μs   (16.08 μs .. 16.26 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 16.23 μs   (16.13 μs .. 16.39 μs)
std dev              475.1 ns   (241.3 ns .. 740.6 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking 2020.23.2
time                 1.690 s    (NaN s .. 1.715 s)
                     1.000 R²   (1.000 R² .. NaN R²)
mean                 1.706 s    (1.698 s .. 1.713 s)
std dev              8.475 ms   (3.791 ms .. 10.25 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 2020.24.1
time                 6.508 ms   (6.348 ms .. 6.781 ms)
                     0.991 R²   (0.983 R² .. 0.999 R²)
mean                 6.387 ms   (6.293 ms .. 6.521 ms)
std dev              311.1 μs   (201.0 μs .. 432.3 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking 2020.24.2
time                 802.5 ms   (798.2 ms .. 811.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 810.4 ms   (806.4 ms .. 817.0 ms)
std dev              6.376 ms   (790.1 μs .. 8.089 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 2020.25.1
time                 2.917 ms   (2.884 ms .. 2.952 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 2.995 ms   (2.958 ms .. 3.081 ms)
std dev              177.4 μs   (88.13 μs .. 319.2 μs)
variance introduced by outliers: 40% (moderately inflated)
```

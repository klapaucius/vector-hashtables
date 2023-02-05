# vector-hashtables


[![Hackage](https://img.shields.io/hackage/v/vector-hashtables.svg?label=Hackage)](https://hackage.haskell.org/package/vector-hashtables)
[![Stackage Nightly Version](https://www.stackage.org/package/vector-hashtables/badge/nightly?label=Stackage/Nightly)](https://www.stackage.org/package/vector-hashtables)
[![Stackage LTS Version](https://www.stackage.org/package/vector-hashtables/badge/lts?label=Stackage/LTS)](https://www.stackage.org/package/vector-hashtables)  
[![Build Status](https://github.com/klapaucius/vector-hashtables/workflows/Haskell-CI/badge.svg)](https://github.com/klapaucius/vector-hashtables/actions?query=workflow%3Ahaskell-ci)

```
benchmarking Comparison/1000/insert/hashtables basic
time                 38.91 μs   (38.65 μs .. 39.23 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 40.21 μs   (39.56 μs .. 41.53 μs)
std dev              3.067 μs   (1.758 μs .. 5.396 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking Comparison/1000/insert/vector-hashtables boxed
time                 25.68 μs   (25.34 μs .. 26.04 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 25.86 μs   (25.61 μs .. 26.32 μs)
std dev              1.143 μs   (855.4 ns .. 1.690 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking Comparison/1000/insert/vector-hashtables unboxed keys
time                 26.35 μs   (25.21 μs .. 27.85 μs)
                     0.986 R²   (0.975 R² .. 0.996 R²)
mean                 26.24 μs   (25.58 μs .. 27.13 μs)
std dev              2.476 μs   (1.857 μs .. 3.427 μs)
variance introduced by outliers: 83% (severely inflated)

benchmarking Comparison/1000/insert/vector-hashtables
time                 22.07 μs   (21.75 μs .. 22.36 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 21.87 μs   (21.68 μs .. 22.10 μs)
std dev              712.6 ns   (602.6 ns .. 840.7 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Comparison/1000/insert/mutable vector boxed
time                 3.046 μs   (3.026 μs .. 3.071 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.081 μs   (3.050 μs .. 3.135 μs)
std dev              135.7 ns   (81.76 ns .. 205.2 ns)
variance introduced by outliers: 58% (severely inflated)

benchmarking Comparison/1000/insert/mutable vector
time                 999.2 ns   (985.1 ns .. 1.014 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 992.8 ns   (980.4 ns .. 1.024 μs)
std dev              63.94 ns   (32.65 ns .. 118.9 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking Comparison/1000/insert (resize)/hashtables basic
time                 165.2 μs   (157.5 μs .. 175.6 μs)
                     0.986 R²   (0.975 R² .. 0.999 R²)
mean                 161.5 μs   (158.8 μs .. 166.6 μs)
std dev              11.69 μs   (7.190 μs .. 18.99 μs)
variance introduced by outliers: 68% (severely inflated)

benchmarking Comparison/1000/insert (resize)/vector-hashtables boxed
time                 46.66 μs   (46.13 μs .. 47.17 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 46.32 μs   (46.06 μs .. 46.65 μs)
std dev              962.5 ns   (764.8 ns .. 1.199 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking Comparison/1000/insert (resize)/vector-hashtables unboxed keys
time                 46.91 μs   (45.02 μs .. 49.26 μs)
                     0.991 R²   (0.984 R² .. 0.999 R²)
mean                 45.45 μs   (44.88 μs .. 46.62 μs)
std dev              2.576 μs   (1.633 μs .. 4.527 μs)
variance introduced by outliers: 62% (severely inflated)

benchmarking Comparison/1000/insert (resize)/vector-hashtables
time                 41.94 μs   (41.55 μs .. 42.45 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 42.17 μs   (41.75 μs .. 42.70 μs)
std dev              1.597 μs   (1.186 μs .. 2.039 μs)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Comparison/1000/insert, delete/hashtables basic
time                 75.68 μs   (75.01 μs .. 76.54 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 75.93 μs   (75.38 μs .. 76.58 μs)
std dev              2.069 μs   (1.738 μs .. 2.488 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking Comparison/1000/insert, delete/vector-hashtables
time                 40.85 μs   (40.20 μs .. 41.98 μs)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 41.97 μs   (41.18 μs .. 43.06 μs)
std dev              2.971 μs   (2.164 μs .. 3.842 μs)
variance introduced by outliers: 72% (severely inflated)

benchmarking Comparison/1000/find/hashtables basic
time                 19.88 μs   (19.76 μs .. 20.02 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 19.98 μs   (19.83 μs .. 20.68 μs)
std dev              925.7 ns   (277.3 ns .. 2.032 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking Comparison/1000/find/vector-hashtables
time                 14.03 μs   (13.95 μs .. 14.14 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.05 μs   (13.98 μs .. 14.20 μs)
std dev              353.4 ns   (220.1 ns .. 604.7 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Comparison/1000/find/vector-hashtables (frozen)
time                 12.82 μs   (12.77 μs .. 12.87 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.85 μs   (12.79 μs .. 13.02 μs)
std dev              296.8 ns   (149.9 ns .. 608.0 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking Comparison/1000/lookupIndex/hashtables basic
time                 21.46 μs   (21.15 μs .. 21.83 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 21.07 μs   (20.93 μs .. 21.27 μs)
std dev              569.5 ns   (424.5 ns .. 765.8 ns)
variance introduced by outliers: 28% (moderately inflated)

benchmarking Comparison/1000/lookupIndex/vector-hashtables
time                 14.47 μs   (14.30 μs .. 14.62 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 14.35 μs   (14.22 μs .. 14.49 μs)
std dev              444.7 ns   (370.8 ns .. 588.5 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Comparison/1000/fromList/hashtables basic
time                 167.0 μs   (166.6 μs .. 167.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 167.2 μs   (166.9 μs .. 167.8 μs)
std dev              1.486 μs   (982.6 ns .. 2.227 μs)

benchmarking Comparison/1000/fromList/vector-hashtables
time                 46.81 μs   (46.51 μs .. 47.10 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 46.74 μs   (46.58 μs .. 47.13 μs)
std dev              755.5 ns   (401.2 ns .. 1.348 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Comparison/1000/toList/hashtables basic
time                 11.86 μs   (11.85 μs .. 11.90 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.88 μs   (11.87 μs .. 11.92 μs)
std dev              84.27 ns   (45.86 ns .. 133.1 ns)

benchmarking Comparison/1000/toList/vector-hashtables
time                 13.84 μs   (13.75 μs .. 13.93 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.83 μs   (13.78 μs .. 13.88 μs)
std dev              167.8 ns   (111.9 ns .. 238.2 ns)

benchmarking Comparison/10000/insert/hashtables basic
time                 508.3 μs   (498.3 μs .. 518.3 μs)
                     0.995 R²   (0.989 R² .. 0.998 R²)
mean                 541.8 μs   (519.9 μs .. 602.4 μs)
std dev              118.0 μs   (61.67 μs .. 214.9 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking Comparison/10000/insert/vector-hashtables boxed
time                 249.9 μs   (247.3 μs .. 252.8 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 249.6 μs   (247.9 μs .. 251.5 μs)
std dev              6.103 μs   (5.190 μs .. 7.198 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking Comparison/10000/insert/vector-hashtables unboxed keys
time                 228.9 μs   (226.1 μs .. 233.0 μs)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 234.6 μs   (231.4 μs .. 240.0 μs)
std dev              13.52 μs   (8.957 μs .. 20.81 μs)
variance introduced by outliers: 55% (severely inflated)

benchmarking Comparison/10000/insert/vector-hashtables
time                 205.9 μs   (203.8 μs .. 208.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 206.1 μs   (204.6 μs .. 207.7 μs)
std dev              5.295 μs   (4.282 μs .. 6.831 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking Comparison/10000/insert/mutable vector boxed
time                 39.74 μs   (39.40 μs .. 40.13 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 39.63 μs   (39.39 μs .. 39.94 μs)
std dev              913.7 ns   (732.1 ns .. 1.260 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking Comparison/10000/insert/mutable vector
time                 8.912 μs   (8.849 μs .. 8.978 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.913 μs   (8.868 μs .. 8.984 μs)
std dev              187.5 ns   (139.6 ns .. 251.6 ns)
variance introduced by outliers: 21% (moderately inflated)

benchmarking Comparison/10000/insert (resize)/hashtables basic
time                 1.409 ms   (1.395 ms .. 1.425 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.399 ms   (1.391 ms .. 1.409 ms)
std dev              31.18 μs   (25.99 μs .. 38.49 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking Comparison/10000/insert (resize)/vector-hashtables boxed
time                 511.9 μs   (507.1 μs .. 517.1 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 515.3 μs   (510.9 μs .. 520.4 μs)
std dev              16.47 μs   (11.77 μs .. 22.61 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking Comparison/10000/insert (resize)/vector-hashtables unboxed keys
time                 469.3 μs   (464.0 μs .. 474.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 469.1 μs   (465.1 μs .. 473.4 μs)
std dev              13.57 μs   (11.12 μs .. 17.63 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking Comparison/10000/insert (resize)/vector-hashtables
time                 397.4 μs   (389.8 μs .. 405.4 μs)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 395.9 μs   (390.7 μs .. 404.8 μs)
std dev              21.76 μs   (10.93 μs .. 37.13 μs)
variance introduced by outliers: 50% (severely inflated)

benchmarking Comparison/10000/insert, delete/hashtables basic
time                 961.8 μs   (940.0 μs .. 991.4 μs)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 974.8 μs   (961.4 μs .. 990.4 μs)
std dev              51.11 μs   (39.48 μs .. 77.48 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking Comparison/10000/insert, delete/vector-hashtables
time                 390.5 μs   (386.3 μs .. 394.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 388.3 μs   (386.2 μs .. 391.1 μs)
std dev              8.246 μs   (6.102 μs .. 12.95 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Comparison/10000/find/hashtables basic
time                 198.3 μs   (197.3 μs .. 199.7 μs)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 206.2 μs   (203.0 μs .. 211.8 μs)
std dev              13.78 μs   (8.640 μs .. 23.14 μs)
variance introduced by outliers: 64% (severely inflated)

benchmarking Comparison/10000/find/vector-hashtables
time                 144.4 μs   (142.2 μs .. 146.8 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 145.5 μs   (144.1 μs .. 147.2 μs)
std dev              5.118 μs   (4.170 μs .. 6.713 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking Comparison/10000/find/vector-hashtables (frozen)
time                 127.4 μs   (126.3 μs .. 128.8 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 127.6 μs   (127.0 μs .. 128.4 μs)
std dev              2.436 μs   (1.921 μs .. 3.124 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Comparison/10000/lookupIndex/hashtables basic
time                 201.2 μs   (198.1 μs .. 204.4 μs)
                     0.994 R²   (0.986 R² .. 0.999 R²)
mean                 215.8 μs   (205.8 μs .. 238.5 μs)
std dev              50.42 μs   (21.99 μs .. 91.35 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking Comparison/10000/lookupIndex/vector-hashtables
time                 168.8 μs   (149.7 μs .. 190.9 μs)
                     0.922 R²   (0.875 R² .. 0.987 R²)
mean                 158.5 μs   (151.4 μs .. 171.9 μs)
std dev              30.82 μs   (17.89 μs .. 48.16 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking Comparison/10000/fromList/hashtables basic
time                 1.498 ms   (1.495 ms .. 1.500 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.486 ms   (1.483 ms .. 1.490 ms)
std dev              11.33 μs   (9.989 μs .. 14.19 μs)

benchmarking Comparison/10000/fromList/vector-hashtables
time                 437.5 μs   (434.7 μs .. 442.0 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 435.6 μs   (434.8 μs .. 437.6 μs)
std dev              4.145 μs   (2.185 μs .. 7.752 μs)

benchmarking Comparison/10000/toList/hashtables basic
time                 180.6 μs   (179.2 μs .. 182.7 μs)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 180.7 μs   (179.6 μs .. 183.2 μs)
std dev              5.564 μs   (2.368 μs .. 10.37 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking Comparison/10000/toList/vector-hashtables
time                 308.0 μs   (306.0 μs .. 310.8 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 306.9 μs   (306.1 μs .. 308.2 μs)
std dev              3.495 μs   (2.409 μs .. 5.571 μs)

benchmarking Comparison/100000/insert/hashtables basic
time                 10.78 ms   (10.14 ms .. 11.47 ms)
                     0.981 R²   (0.968 R² .. 0.993 R²)
mean                 10.27 ms   (9.907 ms .. 10.69 ms)
std dev              1.037 ms   (801.2 μs .. 1.404 ms)
variance introduced by outliers: 55% (severely inflated)

benchmarking Comparison/100000/insert/vector-hashtables boxed
time                 7.378 ms   (7.178 ms .. 7.568 ms)
                     0.989 R²   (0.978 R² .. 0.995 R²)
mean                 7.406 ms   (7.188 ms .. 7.717 ms)
std dev              717.3 μs   (511.6 μs .. 977.4 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarking Comparison/100000/insert/vector-hashtables unboxed keys
time                 5.746 ms   (5.562 ms .. 5.909 ms)
                     0.991 R²   (0.986 R² .. 0.995 R²)
mean                 5.662 ms   (5.496 ms .. 5.886 ms)
std dev              581.5 μs   (442.4 μs .. 834.2 μs)
variance introduced by outliers: 63% (severely inflated)

benchmarking Comparison/100000/insert/vector-hashtables
time                 2.358 ms   (2.270 ms .. 2.452 ms)
                     0.980 R²   (0.970 R² .. 0.988 R²)
mean                 2.401 ms   (2.318 ms .. 2.533 ms)
std dev              345.3 μs   (242.2 μs .. 535.8 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking Comparison/100000/insert/mutable vector boxed
time                 2.058 ms   (1.992 ms .. 2.119 ms)
                     0.984 R²   (0.972 R² .. 0.991 R²)
mean                 2.036 ms   (1.954 ms .. 2.155 ms)
std dev              308.0 μs   (226.0 μs .. 446.1 μs)
variance introduced by outliers: 83% (severely inflated)

benchmarking Comparison/100000/insert/mutable vector
time                 87.63 μs   (86.94 μs .. 88.32 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 87.65 μs   (87.10 μs .. 88.31 μs)
std dev              1.928 μs   (1.609 μs .. 2.504 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking Comparison/100000/insert (resize)/hashtables basic
time                 22.66 ms   (22.02 ms .. 23.25 ms)
                     0.995 R²   (0.989 R² .. 0.998 R²)
mean                 22.77 ms   (22.18 ms .. 23.40 ms)
std dev              1.398 ms   (1.082 ms .. 1.831 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking Comparison/100000/insert (resize)/vector-hashtables boxed
time                 12.87 ms   (12.33 ms .. 13.40 ms)
                     0.988 R²   (0.978 R² .. 0.994 R²)
mean                 12.59 ms   (12.09 ms .. 13.00 ms)
std dev              1.168 ms   (825.4 μs .. 1.670 ms)
variance introduced by outliers: 47% (moderately inflated)

benchmarking Comparison/100000/insert (resize)/vector-hashtables unboxed keys
time                 10.16 ms   (9.768 ms .. 10.60 ms)
                     0.988 R²   (0.981 R² .. 0.994 R²)
mean                 9.993 ms   (9.541 ms .. 10.35 ms)
std dev              1.102 ms   (758.4 μs .. 1.623 ms)
variance introduced by outliers: 58% (severely inflated)

benchmarking Comparison/100000/insert (resize)/vector-hashtables
time                 6.202 ms   (5.965 ms .. 6.467 ms)
                     0.987 R²   (0.980 R² .. 0.992 R²)
mean                 6.151 ms   (5.999 ms .. 6.429 ms)
std dev              606.8 μs   (409.8 μs .. 1.002 ms)
variance introduced by outliers: 59% (severely inflated)

benchmarking Comparison/100000/insert, delete/hashtables basic
time                 14.78 ms   (14.31 ms .. 15.29 ms)
                     0.988 R²   (0.975 R² .. 0.995 R²)
mean                 14.66 ms   (14.22 ms .. 15.13 ms)
std dev              1.165 ms   (886.6 μs .. 1.561 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking Comparison/100000/insert, delete/vector-hashtables
time                 4.196 ms   (4.035 ms .. 4.329 ms)
                     0.986 R²   (0.974 R² .. 0.993 R²)
mean                 4.220 ms   (4.122 ms .. 4.392 ms)
std dev              400.7 μs   (246.1 μs .. 636.6 μs)
variance introduced by outliers: 60% (severely inflated)

benchmarking Comparison/100000/find/hashtables basic
time                 1.982 ms   (1.970 ms .. 2.000 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.989 ms   (1.980 ms .. 2.001 ms)
std dev              35.28 μs   (28.03 μs .. 49.47 μs)

benchmarking Comparison/100000/find/vector-hashtables
time                 1.407 ms   (1.393 ms .. 1.418 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.403 ms   (1.397 ms .. 1.416 ms)
std dev              27.15 μs   (17.89 μs .. 44.48 μs)

benchmarking Comparison/100000/find/vector-hashtables (frozen)
time                 1.268 ms   (1.262 ms .. 1.274 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.281 ms   (1.273 ms .. 1.292 ms)
std dev              30.55 μs   (18.11 μs .. 49.86 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Comparison/100000/lookupIndex/hashtables basic
time                 2.373 ms   (2.225 ms .. 2.521 ms)
                     0.972 R²   (0.957 R² .. 0.988 R²)
mean                 2.237 ms   (2.172 ms .. 2.335 ms)
std dev              267.0 μs   (194.6 μs .. 359.7 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking Comparison/100000/lookupIndex/vector-hashtables
time                 1.414 ms   (1.410 ms .. 1.421 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.419 ms   (1.415 ms .. 1.422 ms)
std dev              11.58 μs   (8.924 μs .. 15.33 μs)

benchmarking Comparison/100000/fromList/hashtables basic
time                 20.81 ms   (20.05 ms .. 22.64 ms)
                     0.985 R²   (0.964 R² .. 1.000 R²)
mean                 20.19 ms   (19.97 ms .. 21.22 ms)
std dev              947.6 μs   (85.26 μs .. 1.999 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking Comparison/100000/fromList/vector-hashtables
time                 5.839 ms   (5.151 ms .. 6.424 ms)
                     0.881 R²   (0.801 R² .. 0.936 R²)
mean                 5.539 ms   (5.131 ms .. 6.241 ms)
std dev              1.524 ms   (1.059 ms .. 2.189 ms)
variance introduced by outliers: 92% (severely inflated)

benchmarking Comparison/100000/toList/hashtables basic
time                 6.273 ms   (5.820 ms .. 6.908 ms)
                     0.958 R²   (0.919 R² .. 1.000 R²)
mean                 5.884 ms   (5.753 ms .. 6.247 ms)
std dev              573.7 μs   (303.4 μs .. 1.114 ms)
variance introduced by outliers: 59% (severely inflated)

benchmarking Comparison/100000/toList/vector-hashtables
time                 9.543 ms   (9.023 ms .. 10.54 ms)
                     0.950 R²   (0.899 R² .. 0.997 R²)
mean                 9.244 ms   (8.973 ms .. 9.883 ms)
std dev              1.062 ms   (424.3 μs .. 1.792 ms)
variance introduced by outliers: 63% (severely inflated)

benchmarking Comparison/1000000/insert/hashtables basic
time                 101.7 ms   (95.97 ms .. 105.4 ms)
                     0.996 R²   (0.988 R² .. 1.000 R²)
mean                 101.1 ms   (98.80 ms .. 104.9 ms)
std dev              4.771 ms   (2.658 ms .. 7.959 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking Comparison/1000000/insert/vector-hashtables boxed
time                 83.26 ms   (81.36 ms .. 84.79 ms)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 82.90 ms   (81.79 ms .. 84.60 ms)
std dev              2.347 ms   (1.267 ms .. 3.740 ms)

benchmarking Comparison/1000000/insert/vector-hashtables unboxed keys
time                 61.02 ms   (58.60 ms .. 63.35 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 60.33 ms   (57.50 ms .. 61.81 ms)
std dev              3.690 ms   (1.587 ms .. 6.637 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Comparison/1000000/insert/vector-hashtables
time                 25.63 ms   (24.93 ms .. 26.30 ms)
                     0.996 R²   (0.991 R² .. 0.998 R²)
mean                 25.59 ms   (24.82 ms .. 26.34 ms)
std dev              1.693 ms   (1.170 ms .. 2.724 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking Comparison/1000000/insert/mutable vector boxed
time                 34.80 ms   (33.16 ms .. 36.42 ms)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 35.03 ms   (33.63 ms .. 35.97 ms)
std dev              2.381 ms   (1.429 ms .. 3.737 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking Comparison/1000000/insert/mutable vector
time                 1.420 ms   (1.283 ms .. 1.589 ms)
                     0.936 R²   (0.900 R² .. 0.970 R²)
mean                 1.360 ms   (1.290 ms .. 1.456 ms)
std dev              252.9 μs   (209.8 μs .. 344.6 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking Comparison/1000000/insert (resize)/hashtables basic
time                 282.2 ms   (267.1 ms .. 300.9 ms)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 272.8 ms   (259.4 ms .. 281.7 ms)
std dev              13.93 ms   (5.206 ms .. 20.42 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Comparison/1000000/insert (resize)/vector-hashtables boxed
time                 130.5 ms   (125.3 ms .. 135.4 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 127.5 ms   (116.8 ms .. 131.6 ms)
std dev              9.768 ms   (2.293 ms .. 15.96 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking Comparison/1000000/insert (resize)/vector-hashtables unboxed keys
time                 98.64 ms   (93.18 ms .. 104.1 ms)
                     0.995 R²   (0.986 R² .. 0.999 R²)
mean                 95.41 ms   (87.57 ms .. 98.86 ms)
std dev              8.158 ms   (2.788 ms .. 12.88 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking Comparison/1000000/insert (resize)/vector-hashtables
time                 50.70 ms   (48.53 ms .. 52.56 ms)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 50.26 ms   (46.79 ms .. 52.22 ms)
std dev              4.736 ms   (2.266 ms .. 8.080 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking Comparison/1000000/insert, delete/hashtables basic
time                 150.6 ms   (142.2 ms .. 159.1 ms)
                     0.995 R²   (0.983 R² .. 0.999 R²)
mean                 148.2 ms   (141.4 ms .. 154.9 ms)
std dev              9.927 ms   (6.353 ms .. 15.02 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Comparison/1000000/insert, delete/vector-hashtables
time                 43.85 ms   (42.67 ms .. 45.16 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 43.91 ms   (43.17 ms .. 45.42 ms)
std dev              2.058 ms   (1.052 ms .. 3.472 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Comparison/1000000/find/hashtables basic
time                 19.90 ms   (19.66 ms .. 20.15 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 19.83 ms   (19.73 ms .. 19.95 ms)
std dev              254.3 μs   (194.8 μs .. 320.2 μs)

benchmarking Comparison/1000000/find/vector-hashtables
time                 14.77 ms   (14.35 ms .. 15.36 ms)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 14.37 ms   (14.23 ms .. 14.59 ms)
std dev              441.9 μs   (272.6 μs .. 624.3 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking Comparison/1000000/find/vector-hashtables (frozen)
time                 12.86 ms   (12.71 ms .. 13.03 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 12.86 ms   (12.79 ms .. 12.95 ms)
std dev              214.6 μs   (162.1 μs .. 260.9 μs)

benchmarking Comparison/1000000/lookupIndex/hashtables basic
time                 23.83 ms   (21.04 ms .. 27.48 ms)
                     0.927 R²   (0.863 R² .. 0.995 R²)
mean                 22.73 ms   (21.38 ms .. 26.29 ms)
std dev              4.433 ms   (2.231 ms .. 7.776 ms)
variance introduced by outliers: 74% (severely inflated)

benchmarking Comparison/1000000/lookupIndex/vector-hashtables
time                 15.29 ms   (12.94 ms .. 17.73 ms)
                     0.844 R²   (0.697 R² .. 0.948 R²)
mean                 17.93 ms   (16.57 ms .. 20.28 ms)
std dev              4.048 ms   (2.744 ms .. 5.619 ms)
variance introduced by outliers: 82% (severely inflated)

benchmarking Comparison/1000000/fromList/hashtables basic
time                 269.2 ms   (242.7 ms .. 308.4 ms)
                     0.991 R²   (0.982 R² .. 1.000 R²)
mean                 248.1 ms   (242.8 ms .. 263.9 ms)
std dev              11.73 ms   (85.78 μs .. 14.59 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Comparison/1000000/fromList/vector-hashtables
time                 61.62 ms   (56.18 ms .. 70.59 ms)
                     0.948 R²   (0.872 R² .. 0.986 R²)
mean                 56.62 ms   (51.73 ms .. 62.25 ms)
std dev              9.794 ms   (7.418 ms .. 12.15 ms)
variance introduced by outliers: 57% (severely inflated)

benchmarking Comparison/1000000/toList/hashtables basic
time                 74.11 ms   (68.99 ms .. 81.85 ms)
                     0.980 R²   (0.950 R² .. 1.000 R²)
mean                 70.93 ms   (68.79 ms .. 74.82 ms)
std dev              4.712 ms   (474.1 μs .. 6.422 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking Comparison/1000000/toList/vector-hashtables
time                 107.4 ms   (99.87 ms .. 113.3 ms)
                     0.991 R²   (0.970 R² .. 1.000 R²)
mean                 104.2 ms   (101.7 ms .. 108.1 ms)
std dev              5.246 ms   (3.609 ms .. 6.996 ms)
variance introduced by outliers: 10% (moderately inflated)

```

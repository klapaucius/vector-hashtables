# vector-hashtables


[![Hackage](https://img.shields.io/hackage/v/vector-hashtables.svg?label=Hackage)](https://hackage.haskell.org/package/vector-hashtables)
[![Stackage Nightly Version](https://www.stackage.org/package/vector-hashtables/badge/nightly?label=Stackage/Nightly)](https://www.stackage.org/package/vector-hashtables)
[![Stackage LTS Version](https://www.stackage.org/package/vector-hashtables/badge/lts?label=Stackage/LTS)](https://www.stackage.org/package/vector-hashtables)
[![Build Status](https://github.com/klapaucius/vector-hashtables/workflows/Haskell-CI/badge.svg)](https://github.com/klapaucius/vector-hashtables/actions?query=workflow%3Ahaskell-ci)

A brief history of this library is given in [this blog post](https://an-pro.org/posts/12-vector-hashtables.html).

## Benchmarks vs `hashtables`

(and `vector` where relevant)

Benchmarks below are produced under GHC 9.2.5, and can be reproduced locally with

```shellsession
cabal bench --benchmark-options="--csv results.csv"
bench-show report results.csv
```
(You will need the `bench-show` tool, which is available from Hackage.)


```
Benchmark                                                          default(ns)
----------------------------------------------------------------- ------------
Comparison/1000/insert/hashtables basic                               47112.15
Comparison/1000/insert/vector-hashtables boxed                        22941.55
Comparison/1000/insert/vector-hashtables unboxed keys                 22338.54
Comparison/1000/insert/vector-hashtables                              19460.33
Comparison/1000/insert/mutable vector boxed                            3216.07
Comparison/1000/insert/mutable vector                                   659.46
Comparison/1000/insert (resize)/hashtables basic                     158026.86
Comparison/1000/insert (resize)/vector-hashtables boxed               42164.21
Comparison/1000/insert (resize)/vector-hashtables unboxed keys        40334.65
Comparison/1000/insert (resize)/vector-hashtables                     37761.08
Comparison/1000/insert, delete/hashtables basic                       85218.96
Comparison/1000/insert, delete/vector-hashtables                      36199.84
Comparison/1000/find/hashtables basic                                 33857.19
Comparison/1000/find/vector-hashtables                                14205.24
Comparison/1000/find/vector-hashtables (frozen)                       12257.88
Comparison/1000/lookupIndex/hashtables basic                          31842.70
Comparison/1000/lookupIndex/vector-hashtables                         14140.17
Comparison/1000/fromList/hashtables basic                            161048.28
Comparison/1000/fromList/vector-hashtables                            48052.28
Comparison/1000/toList/hashtables basic                                9569.82
Comparison/1000/toList/vector-hashtables                              12600.58
Comparison/10000/insert/hashtables basic                             983062.29
Comparison/10000/insert/vector-hashtables boxed                      225218.39
Comparison/10000/insert/vector-hashtables unboxed keys               212758.35
Comparison/10000/insert/vector-hashtables                            186573.02
Comparison/10000/insert/mutable vector boxed                          37415.65
Comparison/10000/insert/mutable vector                                 6956.86
Comparison/10000/insert (resize)/hashtables basic                   1696364.18
Comparison/10000/insert (resize)/vector-hashtables boxed             479122.17
Comparison/10000/insert (resize)/vector-hashtables unboxed keys      422759.56
Comparison/10000/insert (resize)/vector-hashtables                   352441.24
Comparison/10000/insert, delete/hashtables basic                    1426337.13
Comparison/10000/insert, delete/vector-hashtables                    359228.91
Comparison/10000/find/hashtables basic                               330890.67
Comparison/10000/find/vector-hashtables                              141525.60
Comparison/10000/find/vector-hashtables (frozen)                     122542.86
Comparison/10000/lookupIndex/hashtables basic                        327065.33
Comparison/10000/lookupIndex/vector-hashtables                       142678.55
Comparison/10000/fromList/hashtables basic                          1641384.65
Comparison/10000/fromList/vector-hashtables                          542530.31
Comparison/10000/toList/hashtables basic                             143188.55
Comparison/10000/toList/vector-hashtables                            274318.45
Comparison/100000/insert/hashtables basic                          10907947.88
Comparison/100000/insert/vector-hashtables boxed                    6932748.81
Comparison/100000/insert/vector-hashtables unboxed keys             5307166.72
Comparison/100000/insert/vector-hashtables                          2368482.76
Comparison/100000/insert/mutable vector boxed                       1794351.95
Comparison/100000/insert/mutable vector                               69989.51
Comparison/100000/insert (resize)/hashtables basic                 22232168.04
Comparison/100000/insert (resize)/vector-hashtables boxed          11420001.24
Comparison/100000/insert (resize)/vector-hashtables unboxed keys    9496935.97
Comparison/100000/insert (resize)/vector-hashtables                 5988321.89
Comparison/100000/insert, delete/hashtables basic                  15119684.92
Comparison/100000/insert, delete/vector-hashtables                  4058662.48
Comparison/100000/find/hashtables basic                             3473800.70
Comparison/100000/find/vector-hashtables                            1431873.00
Comparison/100000/find/vector-hashtables (frozen)                   1225155.68
Comparison/100000/lookupIndex/hashtables basic                      3235041.49
Comparison/100000/lookupIndex/vector-hashtables                     1439338.81
Comparison/100000/fromList/hashtables basic                        22917643.59
Comparison/100000/fromList/vector-hashtables                        8603353.43
Comparison/100000/toList/hashtables basic                           5336633.37
Comparison/100000/toList/vector-hashtables                          9377042.71
Comparison/1000000/insert/hashtables basic                        109193301.77
Comparison/1000000/insert/vector-hashtables boxed                  77176586.83
Comparison/1000000/insert/vector-hashtables unboxed keys           59037764.29
Comparison/1000000/insert/vector-hashtables                        28535981.51
Comparison/1000000/insert/mutable vector boxed                     29709444.79
Comparison/1000000/insert/mutable vector                             866220.55
Comparison/1000000/insert (resize)/hashtables basic               260585504.79
Comparison/1000000/insert (resize)/vector-hashtables boxed        127608418.79
Comparison/1000000/insert (resize)/vector-hashtables unboxed keys  96059752.01
Comparison/1000000/insert (resize)/vector-hashtables               57038345.24
Comparison/1000000/insert, delete/hashtables basic                153182769.34
Comparison/1000000/insert, delete/vector-hashtables                47664402.90
Comparison/1000000/find/hashtables basic                           35821229.20
Comparison/1000000/find/vector-hashtables                          15711648.56
Comparison/1000000/find/vector-hashtables (frozen)                 12885372.25
Comparison/1000000/lookupIndex/hashtables basic                    32210529.62
Comparison/1000000/lookupIndex/vector-hashtables                   14911308.87
Comparison/1000000/fromList/hashtables basic                      446988819.28
Comparison/1000000/fromList/vector-hashtables                      92793579.75
Comparison/1000000/toList/hashtables basic                         66679253.65
Comparison/1000000/toList/vector-hashtables                       105470198.31
```

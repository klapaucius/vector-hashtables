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
Comparison/1000/insert/hashtables basic                               36907.68
Comparison/1000/insert/vector-hashtables boxed                        17055.86
Comparison/1000/insert/vector-hashtables unboxed keys                 15333.05
Comparison/1000/insert/vector-hashtables                              13881.72
Comparison/1000/insert/mutable vector boxed                            3658.16
Comparison/1000/insert/mutable vector                                  1282.13
Comparison/1000/insert (resize)/hashtables basic                     159834.87
Comparison/1000/insert (resize)/vector-hashtables boxed               25900.28
Comparison/1000/insert (resize)/vector-hashtables unboxed keys        25382.56
Comparison/1000/insert (resize)/vector-hashtables                     21393.31
Comparison/1000/insert, delete/hashtables basic                       72615.51
Comparison/1000/insert, delete/vector-hashtables                      20920.26
Comparison/1000/find/hashtables basic                                 23678.93
Comparison/1000/find/vector-hashtables                                 5586.93
Comparison/1000/find/vector-hashtables (frozen)                        3128.18
Comparison/1000/lookupIndex/hashtables basic                          19209.79
Comparison/1000/lookupIndex/vector-hashtables                          4978.75
Comparison/1000/fromList/hashtables basic                            162920.75
Comparison/1000/fromList/vector-hashtables                            25283.17
Comparison/1000/toList/hashtables basic                                9286.65
Comparison/1000/toList/vector-hashtables                              10022.51
Comparison/10000/insert/hashtables basic                             373885.01
Comparison/10000/insert/vector-hashtables boxed                      175892.73
Comparison/10000/insert/vector-hashtables unboxed keys               147689.46
Comparison/10000/insert/vector-hashtables                            128417.71
Comparison/10000/insert/mutable vector boxed                          42712.36
Comparison/10000/insert/mutable vector                                12193.85
Comparison/10000/insert (resize)/hashtables basic                   1378595.70
Comparison/10000/insert (resize)/vector-hashtables boxed             273187.43
Comparison/10000/insert (resize)/vector-hashtables unboxed keys      242659.06
Comparison/10000/insert (resize)/vector-hashtables                   188011.22
Comparison/10000/insert, delete/hashtables basic                     736250.66
Comparison/10000/insert, delete/vector-hashtables                    200643.92
Comparison/10000/find/hashtables basic                               232874.24
Comparison/10000/find/vector-hashtables                               55693.39
Comparison/10000/find/vector-hashtables (frozen)                      31291.02
Comparison/10000/lookupIndex/hashtables basic                        191707.57
Comparison/10000/lookupIndex/vector-hashtables                        49762.50
Comparison/10000/fromList/hashtables basic                          1562065.86
Comparison/10000/fromList/vector-hashtables                          233128.12
Comparison/10000/toList/hashtables basic                             114543.83
Comparison/10000/toList/vector-hashtables                            154034.89
Comparison/100000/insert/hashtables basic                           4546924.57
Comparison/100000/insert/vector-hashtables boxed                    1767261.44
Comparison/100000/insert/vector-hashtables unboxed keys             1476174.02
Comparison/100000/insert/vector-hashtables                          1272101.52
Comparison/100000/insert/mutable vector boxed                        666673.13
Comparison/100000/insert/mutable vector                              123138.53
Comparison/100000/insert (resize)/hashtables basic                 18777839.68
Comparison/100000/insert (resize)/vector-hashtables boxed           7287443.14
Comparison/100000/insert (resize)/vector-hashtables unboxed keys    5698107.38
Comparison/100000/insert (resize)/vector-hashtables                 2574932.04
Comparison/100000/insert, delete/hashtables basic                   8790937.73
Comparison/100000/insert, delete/vector-hashtables                  2010559.30
Comparison/100000/find/hashtables basic                             2346364.66
Comparison/100000/find/vector-hashtables                             588427.30
Comparison/100000/find/vector-hashtables (frozen)                    319639.97
Comparison/100000/lookupIndex/hashtables basic                      1931036.13
Comparison/100000/lookupIndex/vector-hashtables                      499992.98
Comparison/100000/fromList/hashtables basic                        30059346.94
Comparison/100000/fromList/vector-hashtables                        3945839.47
Comparison/100000/toList/hashtables basic                           2702739.12
Comparison/100000/toList/vector-hashtables                          5118781.70
Comparison/1000000/insert/hashtables basic                         85752701.43
Comparison/1000000/insert/vector-hashtables boxed                  75667649.90
Comparison/1000000/insert/vector-hashtables unboxed keys           46615543.58
Comparison/1000000/insert/vector-hashtables                        16025927.63
Comparison/1000000/insert/mutable vector boxed                     32068295.04
Comparison/1000000/insert/mutable vector                            1393859.51
Comparison/1000000/insert (resize)/hashtables basic               282346897.14
Comparison/1000000/insert (resize)/vector-hashtables boxed         84385042.40
Comparison/1000000/insert (resize)/vector-hashtables unboxed keys  62413398.66
Comparison/1000000/insert (resize)/vector-hashtables               28777902.04
Comparison/1000000/insert, delete/hashtables basic                134399640.44
Comparison/1000000/insert, delete/vector-hashtables                23648387.28
Comparison/1000000/find/hashtables basic                           24583079.42
Comparison/1000000/find/vector-hashtables                           6178348.57
Comparison/1000000/find/vector-hashtables (frozen)                  3425505.60
Comparison/1000000/lookupIndex/hashtables basic                    19753759.16
Comparison/1000000/lookupIndex/vector-hashtables                    5357116.98
Comparison/1000000/fromList/hashtables basic                      222974094.62
Comparison/1000000/fromList/vector-hashtables                      49212505.34
Comparison/1000000/toList/hashtables basic                         66351583.99
Comparison/1000000/toList/vector-hashtables                        98441804.39
```

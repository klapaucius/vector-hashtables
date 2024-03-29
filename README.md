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


| Benchmark | v.0.1.1.4 (ns) | current (ns) |
| ----------------------------------------------------------------- | ------------ | ------------ | 
| Comparison/1000/insert/hashtables basic                              | 36083.57 | 36907.68 |
| Comparison/1000/insert/vector-hashtables boxed                       | 25735.13 | 17055.86 |
| Comparison/1000/insert/vector-hashtables unboxed keys                | 23601.08 | 15333.05 |
| Comparison/1000/insert/vector-hashtables                             | 25298.67 | 13881.72 |
| Comparison/1000/insert/mutable vector boxed                           | 6458.14 | 3658.16 |
| Comparison/1000/insert/mutable vector                                 | 2526.35 | 1282.13 |
| Comparison/1000/insert (resize)/hashtables basic                    | 318207.51 | 159834.87 |
| Comparison/1000/insert (resize)/vector-hashtables boxed             | 106490.84 | 25900.28 |
| Comparison/1000/insert (resize)/vector-hashtables unboxed keys       | 66848.40 | 25382.56 |
| Comparison/1000/insert (resize)/vector-hashtables                    | 65694.66 | 21393.31 |
| Comparison/1000/insert, delete/hashtables basic                      | 96776.41 | 72615.51 |
| Comparison/1000/insert, delete/vector-hashtables                     | 79701.44 | 20920.26 |
| Comparison/1000/find/hashtables basic                                | 22841.85 | 23678.93 |
| Comparison/1000/find/vector-hashtables                               | 16398.19 | 5586.93 |
| Comparison/1000/find/vector-hashtables (frozen)                      | 11967.63 | 3128.18 |
| Comparison/1000/lookupIndex/hashtables basic                         | 19744.27 | 19209.79 |
| Comparison/1000/lookupIndex/vector-hashtables                        | 13624.09 | 4978.75 |
| Comparison/1000/fromList/hashtables basic                           | 167277.66 | 162920.75 |
| Comparison/1000/fromList/vector-hashtables                           | 48670.75 | 25283.17 |
| Comparison/1000/toList/hashtables basic                               | 9296.91 | 9286.65 |
| Comparison/1000/toList/vector-hashtables                              | 9753.58 | 10022.51 |
| Comparison/10000/insert/hashtables basic                            | 384031.02 | 373885.01 |
| Comparison/10000/insert/vector-hashtables boxed                     | 246923.90 | 175892.73 |
| Comparison/10000/insert/vector-hashtables unboxed keys              | 229812.05 | 147689.46 |
| Comparison/10000/insert/vector-hashtables                           | 216924.38 | 128417.71 |
| Comparison/10000/insert/mutable vector boxed                         | 43104.20 | 42712.36 |
| Comparison/10000/insert/mutable vector                               | 12298.81 | 12193.85 |
| Comparison/10000/insert (resize)/hashtables basic                  | 1342541.64 | 1378595.70 |
| Comparison/10000/insert (resize)/vector-hashtables boxed            | 487188.49 | 273187.43 |
| Comparison/10000/insert (resize)/vector-hashtables unboxed keys     | 441635.60 | 242659.06 |
| Comparison/10000/insert (resize)/vector-hashtables                  | 412651.12 | 188011.22 |
| Comparison/10000/insert, delete/hashtables basic                    | 722883.16 | 736250.66 |
| Comparison/10000/insert, delete/vector-hashtables                   | 407113.19 | 200643.92 |
| Comparison/10000/find/hashtables basic                              | 228154.09 | 232874.24 |
| Comparison/10000/find/vector-hashtables                             | 164343.06 | 55693.39 |
| Comparison/10000/find/vector-hashtables (frozen)                    | 119669.00 | 31291.02 |
| Comparison/10000/lookupIndex/hashtables basic                       | 197212.48 | 191707.57 |
| Comparison/10000/lookupIndex/vector-hashtables                      | 136205.70 | 49762.50 |
| Comparison/10000/fromList/hashtables basic                         | 1391968.88 | 1562065.86 |
| Comparison/10000/fromList/vector-hashtables                         | 430590.93 | 233128.12 |
| Comparison/10000/toList/hashtables basic                            | 112894.13 | 114543.83 |
| Comparison/10000/toList/vector-hashtables                           | 152214.13 | 154034.89 |
| Comparison/100000/insert/hashtables basic                          | 4492224.86 | 4546924.57 |
| Comparison/100000/insert/vector-hashtables boxed                   | 2414664.98 | 1767261.44 |
| Comparison/100000/insert/vector-hashtables unboxed keys            | 2243027.45 | 1476174.02 |
| Comparison/100000/insert/vector-hashtables                         | 2144269.70 | 1272101.52 |
| Comparison/100000/insert/mutable vector boxed                       | 667945.33 | 666673.13 |
| Comparison/100000/insert/mutable vector                             | 121264.87 | 123138.53 |
| Comparison/100000/insert (resize)/hashtables basic                | 18217163.16 | 18777839.68 |
| Comparison/100000/insert (resize)/vector-hashtables boxed          | 9452674.69 | 7287443.14 |
| Comparison/100000/insert (resize)/vector-hashtables unboxed keys   | 7721635.33 | 5698107.38 |
| Comparison/100000/insert (resize)/vector-hashtables                | 4722092.98 | 2574932.04 |
| Comparison/100000/insert, delete/hashtables basic                  | 8699786.66 | 8790937.73 |
| Comparison/100000/insert, delete/vector-hashtables                 | 4073127.47 | 2010559.30 |
| Comparison/100000/find/hashtables basic                            | 2283995.01 | 2346364.66 |
| Comparison/100000/find/vector-hashtables                           | 1676135.49 | 588427.30 |
| Comparison/100000/find/vector-hashtables (frozen)                  | 1201572.29 | 319639.97 |
| Comparison/100000/lookupIndex/hashtables basic                     | 1963727.73 | 1931036.13 |
| Comparison/100000/lookupIndex/vector-hashtables                    | 1363501.76 | 499992.98 |
| Comparison/100000/fromList/hashtables basic                       | 20681183.31 | 30059346.94 |
| Comparison/100000/fromList/vector-hashtables                       | 5262183.79 | 3945839.47 |
| Comparison/100000/toList/hashtables basic                          | 2675794.96 | 2702739.12 |
| Comparison/100000/toList/vector-hashtables                         | 5155629.15 | 5118781.70 |
| Comparison/1000000/insert/hashtables basic                        | 86723317.72 | 85752701.43 |
| Comparison/1000000/insert/vector-hashtables boxed                 | 68162021.23 | 75667649.90 |
| Comparison/1000000/insert/vector-hashtables unboxed keys          | 50777620.44 | 46615543.58 |
| Comparison/1000000/insert/vector-hashtables                       | 23334885.43 | 16025927.63 |
| Comparison/1000000/insert/mutable vector boxed                    | 30281652.62 | 32068295.04 |
| Comparison/1000000/insert/mutable vector                           | 1283399.43 | 1393859.51 |
| Comparison/1000000/insert (resize)/hashtables basic              | 228726522.46 | 282346897.14 |
| Comparison/1000000/insert (resize)/vector-hashtables boxed       | 104556190.01 | 84385042.40 |
| Comparison/1000000/insert (resize)/vector-hashtables unboxed keys | 79183320.30 | 62413398.66 |
| Comparison/1000000/insert (resize)/vector-hashtables              | 45925222.08 | 28777902.04 |
| Comparison/1000000/insert, delete/hashtables basic               | 130189177.30 | 134399640.44 |
| Comparison/1000000/insert, delete/vector-hashtables               | 42722592.04 | 23648387.28 |
| Comparison/1000000/find/hashtables basic                          | 23094297.73 | 24583079.42 |
| Comparison/1000000/find/vector-hashtables                         | 16709242.48 | 6178348.57 |
| Comparison/1000000/find/vector-hashtables (frozen)                | 12176361.82 | 3425505.60 |
| Comparison/1000000/lookupIndex/hashtables basic                   | 20222788.08 | 19753759.16 |
| Comparison/1000000/lookupIndex/vector-hashtables                  | 14041315.59 | 5357116.98 |
| Comparison/1000000/fromList/hashtables basic                     | 210947448.60 | 222974094.62 |
| Comparison/1000000/fromList/vector-hashtables                     | 56875691.60 | 49212505.34 |
| Comparison/1000000/toList/hashtables basic                        | 62256321.15 | 66351583.99 |
| Comparison/1000000/toList/vector-hashtables                       | 95883670.57 | 98441804.39 |

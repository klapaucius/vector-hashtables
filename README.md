# vector-hashtables


[![Hackage](https://img.shields.io/hackage/v/vector-hashtables.svg?label=Hackage)](https://hackage.haskell.org/package/vector-hashtables)
[![Stackage Nightly Version](https://www.stackage.org/package/vector-hashtables/badge/nightly?label=Stackage/Nightly)](https://www.stackage.org/package/vector-hashtables)
[![Stackage LTS Version](https://www.stackage.org/package/vector-hashtables/badge/lts?label=Stackage/LTS)](https://www.stackage.org/package/vector-hashtables)
[![Build Status](https://github.com/klapaucius/vector-hashtables/workflows/Haskell-CI/badge.svg)](https://github.com/klapaucius/vector-hashtables/actions?query=workflow%3Ahaskell-ci)

A brief history of this library is given in [this blog post](https://an-pro.org/posts/12-vector-hashtables.html).

## Benchmarks vs `hashtables`

(and `vector` where relevant)

Benchmarks below are produced under GHC 9.4.8, and can be reproduced locally with

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

<details><summary>Utilities benchmark:</summary>

| Benchmark | v.0.1.1.4 (ns) | current (ns) |
| --------- | -------------- | ------------ |
| Utilities/1000/at'                                                   | 14554.08 | 4755.41 |
| Utilities/1000/insert                                                | 16704.55 | 6842.45 |
| Utilities/1000/delete                                                | 11166.80 | 3959.44 |
| Utilities/1000/lookup                                                | 14510.59 | 5161.87 |
| Utilities/1000/lookup'                                               | 14181.87 | 4739.19 |
| Utilities/1000/lookupIndex                                           | 17171.76 | 4196.40 |
| Utilities/1000/null                                                      | 7.19 | 7.92 |
| Utilities/1000/length                                                    | 7.31 | 7.41 |
| Utilities/1000/size                                                      | 6.85 | 6.53 |
| Utilities/1000/member                                                | 17364.48 | 4344.27 |
| Utilities/1000/findWithDefault                                       | 15747.10 | 4802.97 |
| Utilities/1000/upsert                                                | 32633.25 | 12708.79 |
| Utilities/1000/alter                                                 | 31908.54 | 11109.48 |
| Utilities/1000/alterM                                                | 32564.14 | 11110.53 |
| Utilities/1000/union                                                 | 46432.10 | 29468.82 |
| Utilities/1000/difference                                            | 25741.68 | 16296.53 |
| Utilities/1000/intersection                                          | 58828.38 | 38587.57 |
| Utilities/1000/fromList                                              | 45355.85 | 26156.93 |
| Utilities/1000/toList                                                 | 9626.89 | 10101.05 |
| Utilities/10000/at'                                                 | 147509.66 | 48699.88 |
| Utilities/10000/insert                                              | 171201.56 | 68568.34 |
| Utilities/10000/delete                                              | 111650.42 | 39392.43 |
| Utilities/10000/lookup                                              | 149138.47 | 49800.41 |
| Utilities/10000/lookup'                                             | 144283.31 | 46886.69 |
| Utilities/10000/lookupIndex                                         | 172630.09 | 40088.94 |
| Utilities/10000/null                                                     | 7.20 | 7.24 |
| Utilities/10000/length                                                   | 7.30 | 6.77 |
| Utilities/10000/size                                                     | 6.87 | 6.43 |
| Utilities/10000/member                                              | 170650.68 | 43369.07 |
| Utilities/10000/findWithDefault                                     | 157236.92 | 49471.03 |
| Utilities/10000/upsert                                              | 329212.06 | 125290.78 |
| Utilities/10000/alter                                               | 322814.62 | 111817.84 |
| Utilities/10000/alterM                                              | 330094.30 | 112444.63 |
| Utilities/10000/union                                               | 478541.46 | 329790.79 |
| Utilities/10000/difference                                          | 295042.17 | 193790.26 |
| Utilities/10000/intersection                                        | 644396.71 | 419483.32 |
| Utilities/10000/fromList                                            | 494164.34 | 331449.21 |
| Utilities/10000/toList                                              | 151375.79 | 167580.99 |
| Utilities/100000/at'                                               | 1491045.70 | 495418.68 |
| Utilities/100000/insert                                            | 1741058.94 | 765507.51 |
| Utilities/100000/delete                                            | 1127146.84 | 436707.82 |
| Utilities/100000/lookup                                            | 1601916.69 | 562205.51 |
| Utilities/100000/lookup'                                           | 1441526.57 | 488540.28 |
| Utilities/100000/lookupIndex                                       | 1763172.42 | 405596.28 |
| Utilities/100000/null                                                    | 7.19 | 7.26 |
| Utilities/100000/length                                                  | 7.38 | 7.17 |
| Utilities/100000/size                                                    | 6.92 | 6.83 |
| Utilities/100000/member                                            | 1740066.09 | 464281.90 |
| Utilities/100000/findWithDefault                                   | 1577458.36 | 489790.85 |
| Utilities/100000/upsert                                            | 3383104.75 | 1265454.18 |
| Utilities/100000/alter                                             | 3329820.09 | 1211692.27 |
| Utilities/100000/alterM                                            | 3356140.57 | 1220060.24 |
| Utilities/100000/union                                             | 5563999.76 | 3705665.39 |
| Utilities/100000/difference                                        | 6372930.19 | 5630405.29 |
| Utilities/100000/intersection                                     | 12353680.59 | 9595098.36 |
| Utilities/100000/fromList                                          | 5161712.37 | 3685646.90 |
| Utilities/100000/toList                                            | 5109243.49 | 5118785.28 |
| Utilities/1000000/at'                                             | 14831244.23 | 5055419.26 |
| Utilities/1000000/insert                                          | 17633535.06 | 7209602.12 |
| Utilities/1000000/delete                                          | 11251853.98 | 4072535.57 |
| Utilities/1000000/lookup                                          | 15169518.90 | 5208497.64 |
| Utilities/1000000/lookup'                                         | 14532451.21 | 4929673.79 |
| Utilities/1000000/lookupIndex                                     | 17529914.96 | 4216663.23 |
| Utilities/1000000/null                                                   | 7.20 | 7.25 |
| Utilities/1000000/length                                                 | 7.30 | 6.81 |
| Utilities/1000000/size                                                   | 6.86 | 6.43 |
| Utilities/1000000/member                                          | 17461069.35 | 4604944.15 |
| Utilities/1000000/findWithDefault                                 | 15945541.78 | 5058608.89 |
| Utilities/1000000/upsert                                          | 34444162.79 | 12052143.01 |
| Utilities/1000000/alter                                           | 33820504.88 | 11842773.92 |
| Utilities/1000000/alterM                                          | 33991841.71 | 11908234.12 |
| Utilities/1000000/union                                           | 59911378.73 | 44470700.12 |
| Utilities/1000000/difference                                     | 117323371.53 | 107670945.26 |
| Utilities/1000000/intersection                                   | 195009586.71 | 161847790.47 |
| Utilities/1000000/fromList                                        | 97086662.77 | 52734408.34 |
| Utilities/1000000/toList                                         | 166554860.99 | 99619875.06 |

</details>

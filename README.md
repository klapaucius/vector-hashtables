# vector-hashtables

```
benchmarking insert hashtables basic
time                 2.241 s    (2.181 s .. 2.305 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.294 s    (2.285 s .. 2.308 s)
std dev              12.24 ms   (0.0 s .. 13.46 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert hashtables cuckoo
time                 5.254 s    (5.110 s .. 5.558 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.345 s    (5.270 s .. 5.396 s)
std dev              77.24 ms   (0.0 s .. 89.11 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert hashtables linear
time                 264.9 s    (137.1 s .. 322.3 s)
                     0.972 R²   (0.927 R² .. 1.000 R²)
mean                 185.9 s    (134.6 s .. 216.7 s)
std dev              47.10 s    (0.0 s .. 53.50 s)
variance introduced by outliers: 48% (moderately inflated)

benchmarking insert vector-hashtables
time                 433.6 ms   (239.0 ms .. 538.4 ms)
                     0.974 R²   (0.941 R² .. 1.000 R²)
mean                 414.7 ms   (390.8 ms .. 431.6 ms)
std dev              25.44 ms   (0.0 s .. 29.23 ms)
variance introduced by outliers: 19% (moderately inflated)
```

```
benchmarking insert hashtables basic
time                 2.691 s    (2.303 s .. 3.227 s)
                     0.994 R²   (0.992 R² .. 1.000 R²)
mean                 2.652 s    (2.597 s .. 2.704 s)
std dev              88.79 ms   (0.0 s .. 90.97 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert vector-hashtables boxed
time                 963.5 ms   (883.6 ms .. NaN s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 1.055 s    (1.021 s .. 1.075 s)
std dev              30.66 ms   (0.0 s .. 33.63 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert vector-hashtables unboxed keys
time                 702.0 ms   (591.0 ms .. 847.0 ms)
                     0.995 R²   (0.984 R² .. 1.000 R²)
mean                 756.9 ms   (740.9 ms .. 770.3 ms)
std dev              21.48 ms   (0.0 s .. 23.33 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert vector-hashtables
time                 254.9 ms   (190.9 ms .. 300.1 ms)
                     0.966 R²   (0.915 R² .. 1.000 R²)
mean                 288.0 ms   (252.5 ms .. 303.4 ms)
std dev              25.77 ms   (9.859 ms .. 35.60 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking insert mutable vector boxed
time                 502.3 ms   (216.4 ms .. 837.4 ms)
                     0.950 R²   (0.852 R² .. 1.000 R²)
mean                 537.6 ms   (494.0 ms .. 608.5 ms)
std dev              61.89 ms   (0.0 s .. 66.09 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking insert mutable vector
time                 26.04 ms   (21.87 ms .. 29.43 ms)
                     0.938 R²   (0.875 R² .. 0.996 R²)
mean                 29.19 ms   (27.55 ms .. 30.57 ms)
std dev              3.253 ms   (2.206 ms .. 4.598 ms)
variance introduced by outliers: 48% (moderately inflated)
```

`-N4 -A64m -n4m -qb0`

```
benchmarking insert hashtables basic
time                 2.859 s    (2.544 s .. 3.039 s)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 2.752 s    (2.721 s .. 2.779 s)
std dev              42.23 ms   (0.0 s .. 46.01 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert vector-hashtables boxed
time                 1.377 s    (1.066 s .. 1.986 s)
                     0.978 R²   (0.957 R² .. 1.000 R²)
mean                 1.225 s    (1.198 s .. 1.250 s)
std dev              41.39 ms   (0.0 s .. 42.85 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert vector-hashtables unboxed keys
time                 948.5 ms   (657.4 ms .. 1.402 s)
                     0.975 R²   (0.961 R² .. 1.000 R²)
mean                 881.1 ms   (846.5 ms .. 941.1 ms)
std dev              52.18 ms   (0.0 s .. 54.60 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking insert vector-hashtables
time                 260.0 ms   (239.1 ms .. 270.3 ms)
                     0.998 R²   (0.990 R² .. 1.000 R²)
mean                 278.3 ms   (271.0 ms .. 291.9 ms)
std dev              13.67 ms   (451.2 us .. 16.27 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking insert mutable vector boxed
time                 116.4 ms   (112.7 ms .. 124.3 ms)
                     0.996 R²   (0.987 R² .. 1.000 R²)
mean                 117.9 ms   (114.4 ms .. 123.0 ms)
std dev              6.494 ms   (3.339 ms .. 10.02 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking insert mutable vector
time                 24.08 ms   (22.62 ms .. 25.97 ms)
                     0.965 R²   (0.904 R² .. 0.994 R²)
mean                 27.13 ms   (25.74 ms .. 29.30 ms)
std dev              3.718 ms   (2.664 ms .. 5.019 ms)
variance introduced by outliers: 59% (severely inflated)
```
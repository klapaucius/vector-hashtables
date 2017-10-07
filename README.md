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

### n = 1000

```
benchmarking insert hashtables cuckoo (resize)
time                 475.7 us   (439.0 us .. 527.7 us)
                     0.964 R?   (0.947 R? .. 0.988 R?)
mean                 512.3 us   (496.5 us .. 534.7 us)
std dev              63.94 us   (51.13 us .. 80.94 us)
variance introduced by outliers: 84% (severely inflated)

benchmarking insert hashtables cuckoo
time                 202.2 us   (194.6 us .. 207.6 us)
                     0.990 R?   (0.985 R? .. 0.993 R?)
mean                 188.0 us   (182.6 us .. 194.1 us)
std dev              20.09 us   (17.11 us .. 26.85 us)
variance introduced by outliers: 82% (severely inflated)

benchmarking insert hashtables basic
time                 71.53 us   (70.04 us .. 73.75 us)
                     0.995 R?   (0.994 R? .. 0.997 R?)
mean                 74.37 us   (72.98 us .. 75.88 us)
std dev              4.675 us   (3.855 us .. 5.838 us)
variance introduced by outliers: 64% (severely inflated)

benchmarking insert hashtables basic (resize)
time                 214.3 us   (208.1 us .. 219.3 us)
                     0.995 R?   (0.993 R? .. 0.997 R?)
mean                 223.6 us   (219.3 us .. 230.5 us)
std dev              16.76 us   (11.79 us .. 23.57 us)
variance introduced by outliers: 68% (severely inflated)

benchmarking insert hashtables basic (delete)
time                 167.9 us   (157.7 us .. 177.4 us)
                     0.980 R?   (0.972 R? .. 0.991 R?)
mean                 159.1 us   (154.7 us .. 163.7 us)
std dev              14.92 us   (11.99 us .. 19.71 us)
variance introduced by outliers: 78% (severely inflated)

benchmarking insert hashtables linear
time                 163.4 us   (148.5 us .. 175.9 us)
                     0.969 R?   (0.955 R? .. 0.984 R?)
mean                 144.1 us   (138.1 us .. 153.4 us)
std dev              23.89 us   (17.58 us .. 34.58 us)
variance introduced by outliers: 92% (severely inflated)

benchmarking insert vector-hashtables boxed
time                 37.75 us   (35.81 us .. 40.58 us)
                     0.975 R?   (0.966 R? .. 0.986 R?)
mean                 40.67 us   (39.18 us .. 42.09 us)
std dev              4.697 us   (3.926 us .. 5.559 us)
variance introduced by outliers: 88% (severely inflated)

benchmarking insert vector-hashtables unboxed keys
time                 32.26 us   (30.90 us .. 33.84 us)
                     0.980 R?   (0.972 R? .. 0.987 R?)
mean                 37.32 us   (35.73 us .. 39.52 us)
std dev              6.377 us   (5.219 us .. 7.844 us)
variance introduced by outliers: 94% (severely inflated)

benchmarking insert vector-hashtables (resize)
time                 55.36 us   (53.82 us .. 56.63 us)
                     0.995 R?   (0.992 R? .. 0.997 R?)
mean                 56.54 us   (55.55 us .. 58.07 us)
std dev              4.192 us   (2.909 us .. 6.831 us)
variance introduced by outliers: 73% (severely inflated)

benchmarking insert vector-hashtables (delete)
time                 63.53 us   (61.91 us .. 64.97 us)
                     0.995 R?   (0.992 R? .. 0.997 R?)
mean                 62.80 us   (61.53 us .. 63.95 us)
std dev              4.058 us   (3.519 us .. 4.837 us)
variance introduced by outliers: 66% (severely inflated)

benchmarking insert vector-hashtables
time                 32.46 us   (31.17 us .. 33.76 us)
                     0.989 R?   (0.985 R? .. 0.994 R?)
mean                 31.06 us   (30.17 us .. 32.12 us)
std dev              3.229 us   (2.763 us .. 3.952 us)
variance introduced by outliers: 85% (severely inflated)

benchmarking insert mutable vector boxed
time                 4.219 us   (4.124 us .. 4.351 us)
                     0.995 R?   (0.994 R? .. 0.997 R?)
mean                 4.364 us   (4.285 us .. 4.436 us)
std dev              252.4 ns   (216.5 ns .. 302.4 ns)
variance introduced by outliers: 69% (severely inflated)

benchmarking insert mutable vector
time                 1.769 us   (1.718 us .. 1.820 us)
                     0.996 R?   (0.994 R? .. 0.998 R?)
mean                 1.769 us   (1.745 us .. 1.797 us)
std dev              86.97 ns   (71.41 ns .. 105.8 ns)
variance introduced by outliers: 64% (severely inflated)

Benchmark vector-hashtables-bench: FINISH
Completed 2 action(s).
PS D:\hp\vector-hashtables>
```
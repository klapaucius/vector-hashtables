# vector-hashtables

```
benchmarking 1000/insert/hashtables basic
time                 39.64 μs   (39.32 μs .. 40.04 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 39.63 μs   (39.36 μs .. 40.05 μs)
std dev              1.103 μs   (771.3 ns .. 1.838 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking 1000/insert/vector-hashtables boxed
time                 27.31 μs   (27.04 μs .. 27.69 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 27.53 μs   (27.25 μs .. 28.01 μs)
std dev              1.176 μs   (768.5 ns .. 1.760 μs)
variance introduced by outliers: 49% (moderately inflated)

benchmarking 1000/insert/vector-hashtables unboxed keys
time                 25.39 μs   (25.21 μs .. 25.60 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 25.77 μs   (25.46 μs .. 26.19 μs)
std dev              1.163 μs   (819.7 ns .. 1.560 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking 1000/insert/vector-hashtables
time                 22.97 μs   (22.78 μs .. 23.15 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 23.07 μs   (22.92 μs .. 23.28 μs)
std dev              580.3 ns   (448.8 ns .. 771.7 ns)
variance introduced by outliers: 25% (moderately inflated)

benchmarking 1000/insert/mutable vector boxed
time                 3.098 μs   (3.067 μs .. 3.131 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.088 μs   (3.068 μs .. 3.121 μs)
std dev              84.19 ns   (60.93 ns .. 117.1 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking 1000/insert/mutable vector
time                 977.8 ns   (968.6 ns .. 988.2 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 977.4 ns   (970.1 ns .. 986.7 ns)
std dev              28.28 ns   (22.62 ns .. 37.43 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking 1000/insert (resize)/hashtables basic
time                 158.7 μs   (157.2 μs .. 160.8 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 157.9 μs   (156.9 μs .. 159.4 μs)
std dev              4.298 μs   (3.266 μs .. 5.501 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking 1000/insert (resize)/vector-hashtables boxed
time                 52.32 μs   (51.35 μs .. 53.45 μs)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 51.93 μs   (51.38 μs .. 52.80 μs)
std dev              2.224 μs   (1.624 μs .. 2.869 μs)
variance introduced by outliers: 47% (moderately inflated)

benchmarking 1000/insert (resize)/vector-hashtables unboxed keys
time                 48.98 μs   (48.43 μs .. 49.69 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 49.16 μs   (48.66 μs .. 49.94 μs)
std dev              1.948 μs   (1.408 μs .. 2.812 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking 1000/insert (resize)/vector-hashtables
time                 45.71 μs   (44.93 μs .. 46.77 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 45.58 μs   (45.20 μs .. 46.07 μs)
std dev              1.391 μs   (1.133 μs .. 1.885 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking 1000/insert, delete/hashtables basic
time                 77.38 μs   (75.84 μs .. 79.54 μs)
                     0.994 R²   (0.984 R² .. 0.999 R²)
mean                 77.14 μs   (76.09 μs .. 79.88 μs)
std dev              5.121 μs   (2.453 μs .. 9.375 μs)
variance introduced by outliers: 67% (severely inflated)

benchmarking 1000/insert, delete/vector-hashtables
time                 42.61 μs   (42.20 μs .. 43.01 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 42.11 μs   (41.79 μs .. 42.46 μs)
std dev              1.102 μs   (888.3 ns .. 1.435 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking 1000/find/hashtables basic
time                 20.81 μs   (20.05 μs .. 21.65 μs)
                     0.994 R²   (0.992 R² .. 1.000 R²)
mean                 20.36 μs   (20.18 μs .. 20.78 μs)
std dev              801.0 ns   (398.7 ns .. 1.303 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking 1000/find/vector-hashtables
time                 15.21 μs   (15.14 μs .. 15.28 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 15.28 μs   (15.18 μs .. 15.53 μs)
std dev              479.4 ns   (263.8 ns .. 855.7 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking 1000/find/vector-hashtables (frozen)
time                 13.91 μs   (13.77 μs .. 14.14 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 13.84 μs   (13.76 μs .. 14.02 μs)
std dev              366.3 ns   (217.5 ns .. 675.0 ns)
variance introduced by outliers: 29% (moderately inflated)

benchmarking 10000/insert/hashtables basic
time                 511.1 μs   (507.2 μs .. 515.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 509.5 μs   (505.3 μs .. 516.2 μs)
std dev              17.20 μs   (11.65 μs .. 28.05 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking 10000/insert/vector-hashtables boxed
time                 273.7 μs   (270.7 μs .. 277.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 274.8 μs   (272.1 μs .. 277.9 μs)
std dev              9.799 μs   (7.572 μs .. 12.30 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking 10000/insert/vector-hashtables unboxed keys
time                 247.0 μs   (244.5 μs .. 250.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 246.6 μs   (244.9 μs .. 248.7 μs)
std dev              6.133 μs   (4.953 μs .. 7.929 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking 10000/insert/vector-hashtables
time                 220.5 μs   (218.3 μs .. 223.5 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 220.9 μs   (219.1 μs .. 224.4 μs)
std dev              9.066 μs   (4.599 μs .. 16.90 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking 10000/insert/mutable vector boxed
time                 40.27 μs   (39.96 μs .. 40.65 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 40.36 μs   (40.07 μs .. 40.75 μs)
std dev              1.121 μs   (841.5 ns .. 1.536 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking 10000/insert/mutable vector
time                 8.998 μs   (8.902 μs .. 9.105 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.040 μs   (8.951 μs .. 9.178 μs)
std dev              355.8 ns   (231.4 ns .. 541.2 ns)
variance introduced by outliers: 48% (moderately inflated)

benchmarking 10000/insert (resize)/hashtables basic
time                 1.449 ms   (1.426 ms .. 1.475 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.429 ms   (1.417 ms .. 1.450 ms)
std dev              53.11 μs   (32.17 μs .. 91.23 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking 10000/insert (resize)/vector-hashtables boxed
time                 571.0 μs   (561.7 μs .. 583.0 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 567.2 μs   (562.4 μs .. 573.1 μs)
std dev              17.46 μs   (14.39 μs .. 22.08 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking 10000/insert (resize)/vector-hashtables unboxed keys
time                 506.8 μs   (501.4 μs .. 512.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 506.9 μs   (503.3 μs .. 510.7 μs)
std dev              13.17 μs   (11.14 μs .. 15.47 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking 10000/insert (resize)/vector-hashtables
time                 426.2 μs   (421.7 μs .. 431.7 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 425.7 μs   (422.5 μs .. 430.2 μs)
std dev              12.44 μs   (9.053 μs .. 17.00 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking 10000/insert, delete/hashtables basic
time                 972.2 μs   (954.2 μs .. 996.5 μs)
                     0.993 R²   (0.986 R² .. 0.998 R²)
mean                 995.8 μs   (980.0 μs .. 1.023 ms)
std dev              64.05 μs   (44.29 μs .. 105.3 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking 10000/insert, delete/vector-hashtables
time                 409.7 μs   (405.5 μs .. 414.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 409.3 μs   (406.8 μs .. 412.6 μs)
std dev              9.732 μs   (7.457 μs .. 13.24 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking 10000/find/hashtables basic
time                 199.1 μs   (198.2 μs .. 200.0 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 199.9 μs   (198.9 μs .. 201.5 μs)
std dev              4.359 μs   (3.215 μs .. 6.604 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 10000/find/vector-hashtables
time                 152.9 μs   (152.0 μs .. 154.0 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 153.8 μs   (152.4 μs .. 157.8 μs)
std dev              6.950 μs   (2.965 μs .. 14.32 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking 10000/find/vector-hashtables (frozen)
time                 138.7 μs   (137.3 μs .. 140.3 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 138.6 μs   (137.8 μs .. 140.6 μs)
std dev              3.740 μs   (2.252 μs .. 6.999 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking 100000/insert/hashtables basic
time                 10.63 ms   (10.34 ms .. 10.91 ms)
                     0.989 R²   (0.979 R² .. 0.995 R²)
mean                 10.55 ms   (10.27 ms .. 10.93 ms)
std dev              879.0 μs   (675.2 μs .. 1.187 ms)
variance introduced by outliers: 43% (moderately inflated)

benchmarking 100000/insert/vector-hashtables boxed
time                 7.646 ms   (7.372 ms .. 7.890 ms)
                     0.987 R²   (0.975 R² .. 0.994 R²)
mean                 7.551 ms   (7.325 ms .. 7.957 ms)
std dev              850.4 μs   (581.5 μs .. 1.331 ms)
variance introduced by outliers: 63% (severely inflated)

benchmarking 100000/insert/vector-hashtables unboxed keys
time                 5.971 ms   (5.762 ms .. 6.198 ms)
                     0.986 R²   (0.977 R² .. 0.993 R²)
mean                 5.889 ms   (5.685 ms .. 6.139 ms)
std dev              665.6 μs   (530.7 μs .. 901.9 μs)
variance introduced by outliers: 64% (severely inflated)

benchmarking 100000/insert/vector-hashtables
time                 2.657 ms   (2.566 ms .. 2.746 ms)
                     0.980 R²   (0.966 R² .. 0.990 R²)
mean                 2.711 ms   (2.593 ms .. 2.917 ms)
std dev              487.6 μs   (310.1 μs .. 743.3 μs)
variance introduced by outliers: 88% (severely inflated)

benchmarking 100000/insert/mutable vector boxed
time                 2.118 ms   (2.050 ms .. 2.178 ms)
                     0.985 R²   (0.975 R² .. 0.992 R²)
mean                 2.082 ms   (2.001 ms .. 2.173 ms)
std dev              294.3 μs   (216.0 μs .. 413.7 μs)
variance introduced by outliers: 82% (severely inflated)

benchmarking 100000/insert/mutable vector
time                 90.01 μs   (88.86 μs .. 91.35 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 89.71 μs   (88.89 μs .. 90.77 μs)
std dev              3.013 μs   (2.269 μs .. 4.313 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking 100000/insert (resize)/hashtables basic
time                 23.62 ms   (23.12 ms .. 24.12 ms)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 23.19 ms   (22.62 ms .. 23.74 ms)
std dev              1.278 ms   (1.012 ms .. 1.564 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking 100000/insert (resize)/vector-hashtables boxed
time                 13.33 ms   (12.77 ms .. 13.90 ms)
                     0.989 R²   (0.981 R² .. 0.994 R²)
mean                 13.14 ms   (12.63 ms .. 13.64 ms)
std dev              1.267 ms   (962.8 μs .. 1.651 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking 100000/insert (resize)/vector-hashtables unboxed keys
time                 10.72 ms   (10.25 ms .. 11.18 ms)
                     0.985 R²   (0.973 R² .. 0.993 R²)
mean                 10.51 ms   (10.03 ms .. 10.96 ms)
std dev              1.218 ms   (840.6 μs .. 1.763 ms)
variance introduced by outliers: 61% (severely inflated)

benchmarking 100000/insert (resize)/vector-hashtables
time                 6.710 ms   (6.480 ms .. 6.944 ms)
                     0.987 R²   (0.974 R² .. 0.993 R²)
mean                 6.564 ms   (6.329 ms .. 6.895 ms)
std dev              807.8 μs   (540.1 μs .. 1.323 ms)
variance introduced by outliers: 68% (severely inflated)

benchmarking 100000/insert, delete/hashtables basic
time                 14.96 ms   (14.59 ms .. 15.33 ms)
                     0.992 R²   (0.983 R² .. 0.996 R²)
mean                 14.62 ms   (14.25 ms .. 15.07 ms)
std dev              1.031 ms   (822.4 μs .. 1.321 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking 100000/insert, delete/vector-hashtables
time                 4.506 ms   (4.357 ms .. 4.621 ms)
                     0.980 R²   (0.961 R² .. 0.991 R²)
mean                 4.552 ms   (4.411 ms .. 4.777 ms)
std dev              585.8 μs   (416.5 μs .. 787.6 μs)
variance introduced by outliers: 73% (severely inflated)

benchmarking 100000/find/hashtables basic
time                 2.017 ms   (1.991 ms .. 2.047 ms)
                     0.989 R²   (0.972 R² .. 0.999 R²)
mean                 2.063 ms   (2.023 ms .. 2.207 ms)
std dev              201.6 μs   (76.22 μs .. 365.8 μs)
variance introduced by outliers: 67% (severely inflated)

benchmarking 100000/find/vector-hashtables
time                 1.529 ms   (1.518 ms .. 1.542 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.533 ms   (1.527 ms .. 1.540 ms)
std dev              21.91 μs   (18.03 μs .. 31.40 μs)

benchmarking 100000/find/vector-hashtables (frozen)
time                 1.394 ms   (1.382 ms .. 1.410 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.385 ms   (1.378 ms .. 1.394 ms)
std dev              27.58 μs   (22.78 μs .. 35.26 μs)

benchmarking 1000000/insert/hashtables basic
time                 109.6 ms   (102.4 ms .. 113.9 ms)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 103.4 ms   (101.2 ms .. 106.1 ms)
std dev              4.027 ms   (2.930 ms .. 5.465 ms)

benchmarking 1000000/insert/vector-hashtables boxed
time                 81.76 ms   (74.52 ms .. 85.91 ms)
                     0.993 R²   (0.982 R² .. 1.000 R²)
mean                 88.14 ms   (85.13 ms .. 94.13 ms)
std dev              7.472 ms   (2.901 ms .. 12.59 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking 1000000/insert/vector-hashtables unboxed keys
time                 63.05 ms   (61.40 ms .. 64.69 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 64.15 ms   (61.84 ms .. 66.15 ms)
std dev              3.972 ms   (2.268 ms .. 6.632 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking 1000000/insert/vector-hashtables
time                 28.69 ms   (27.93 ms .. 29.39 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 28.27 ms   (27.39 ms .. 28.91 ms)
std dev              1.541 ms   (1.018 ms .. 2.291 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking 1000000/insert/mutable vector boxed
time                 34.54 ms   (33.06 ms .. 35.92 ms)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 35.63 ms   (34.50 ms .. 37.30 ms)
std dev              2.704 ms   (1.673 ms .. 4.159 ms)
variance introduced by outliers: 30% (moderately inflated)

benchmarking 1000000/insert/mutable vector
time                 1.249 ms   (1.216 ms .. 1.293 ms)
                     0.989 R²   (0.982 R² .. 0.995 R²)
mean                 1.281 ms   (1.246 ms .. 1.336 ms)
std dev              134.5 μs   (90.70 μs .. 229.5 μs)
variance introduced by outliers: 73% (severely inflated)

benchmarking 1000000/insert (resize)/hashtables basic
time                 283.5 ms   (262.8 ms .. 297.4 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 273.1 ms   (260.1 ms .. 278.9 ms)
std dev              11.48 ms   (4.217 ms .. 15.85 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 1000000/insert (resize)/vector-hashtables boxed
time                 134.2 ms   (129.5 ms .. 140.1 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 133.3 ms   (123.6 ms .. 137.1 ms)
std dev              8.649 ms   (2.261 ms .. 13.69 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking 1000000/insert (resize)/vector-hashtables unboxed keys
time                 101.1 ms   (97.13 ms .. 105.7 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 99.57 ms   (92.30 ms .. 102.5 ms)
std dev              7.264 ms   (2.144 ms .. 11.64 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking 1000000/insert (resize)/vector-hashtables
time                 55.45 ms   (51.91 ms .. 57.64 ms)
                     0.993 R²   (0.981 R² .. 0.998 R²)
mean                 54.58 ms   (50.49 ms .. 56.96 ms)
std dev              5.845 ms   (2.948 ms .. 9.511 ms)
variance introduced by outliers: 40% (moderately inflated)

benchmarking 1000000/insert, delete/hashtables basic
time                 152.7 ms   (143.4 ms .. 166.8 ms)
                     0.992 R²   (0.969 R² .. 0.999 R²)
mean                 153.8 ms   (148.3 ms .. 158.9 ms)
std dev              7.927 ms   (5.077 ms .. 11.73 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking 1000000/insert, delete/vector-hashtables
time                 48.13 ms   (46.71 ms .. 49.42 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 46.50 ms   (45.28 ms .. 47.47 ms)
std dev              2.077 ms   (1.457 ms .. 2.781 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking 1000000/find/hashtables basic
time                 20.23 ms   (20.01 ms .. 20.47 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 20.30 ms   (20.17 ms .. 20.48 ms)
std dev              364.9 μs   (266.4 μs .. 474.7 μs)

benchmarking 1000000/find/vector-hashtables
time                 17.00 ms   (15.45 ms .. 19.30 ms)
                     0.963 R²   (0.940 R² .. 0.999 R²)
mean                 15.72 ms   (15.46 ms .. 17.07 ms)
std dev              1.094 ms   (279.2 μs .. 2.355 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking 1000000/find/vector-hashtables (frozen)
time                 13.87 ms   (13.66 ms .. 14.07 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 14.17 ms   (14.03 ms .. 14.43 ms)
std dev              472.0 μs   (307.6 μs .. 752.0 μs)
variance introduced by outliers: 11% (moderately inflated)
```
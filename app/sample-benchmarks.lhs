-- sample benchmarks -- 23 APR 2021.
-- author: Prem Muthedath
--------------------------------------------------------------------------------

***** benchmark input list: Simple, size: 10  *****
benchmarking quicksort/Classic:
time                 738.3 ns   (730.1 ns .. 749.3 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 746.1 ns   (738.0 ns .. 756.4 ns)
std dev              29.20 ns   (22.72 ns .. 43.41 ns)
variance introduced by outliers: 55% (severely inflated)

benchmarking quicksort/Diller:
time                 1.157 μs   (1.148 μs .. 1.166 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.161 μs   (1.147 μs .. 1.175 μs)
std dev              46.13 ns   (37.55 ns .. 59.53 ns)
variance introduced by outliers: 55% (severely inflated)

benchmarking quicksort/Leal:
time                 492.3 ns   (489.5 ns .. 495.5 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 494.6 ns   (489.2 ns .. 508.3 ns)
std dev              27.31 ns   (15.33 ns .. 47.04 ns)
variance introduced by outliers: 72% (severely inflated)

benchmarking quicksort/LealM:
time                 491.8 ns   (488.2 ns .. 496.0 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 493.1 ns   (487.7 ns .. 500.3 ns)
std dev              21.83 ns   (16.51 ns .. 30.20 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking quicksort/Bird:
time                 485.5 ns   (481.8 ns .. 490.3 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 490.4 ns   (485.5 ns .. 496.1 ns)
std dev              17.83 ns   (14.40 ns .. 22.59 ns)
variance introduced by outliers: 52% (severely inflated)

benchmarking quicksort-split/split:
time                 362.5 ns   (358.8 ns .. 367.2 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 364.4 ns   (361.7 ns .. 367.7 ns)
std dev              10.41 ns   (8.766 ns .. 12.82 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking quicksort-split/split'':
time                 94.28 ns   (93.51 ns .. 95.14 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 94.75 ns   (93.99 ns .. 95.91 ns)
std dev              2.982 ns   (2.235 ns .. 4.359 ns)
variance introduced by outliers: 48% (moderately inflated)

--------------------------------------------------------------------------------

***** benchmark input list: Random, size: 1000000  *****
benchmarking quicksort/Classic:
time                 2.153 s    (2.007 s .. 2.387 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 2.330 s    (2.241 s .. 2.409 s)
std dev              92.87 ms   (74.45 ms .. 106.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Diller:
time                 2.768 s    (2.490 s .. 3.005 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 2.935 s    (2.845 s .. 3.022 s)
std dev              100.6 ms   (84.23 ms .. 107.8 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Leal:
time                 1.292 s    (1.084 s .. 1.393 s)
                     0.997 R²   (0.993 R² .. 1.000 R²)
mean                 1.355 s    (1.303 s .. 1.373 s)
std dev              35.19 ms   (1.011 ms .. 43.12 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/LealM:
time                 1.280 s    (1.106 s .. 1.380 s)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 1.358 s    (1.301 s .. 1.378 s)
std dev              38.77 ms   (849.6 μs .. 47.47 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Bird:
time                 1.294 s    (1.121 s .. 1.398 s)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 1.348 s    (1.315 s .. 1.366 s)
std dev              31.25 ms   (5.458 ms .. 40.68 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort-split/split:
time                 90.32 ms   (84.02 ms .. 95.32 ms)
                     0.990 R²   (0.969 R² .. 0.998 R²)
mean                 83.46 ms   (75.07 ms .. 88.15 ms)
std dev              11.02 ms   (5.469 ms .. 18.26 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking quicksort-split/split'':
time                 72.45 ms   (66.32 ms .. 79.26 ms)
                     0.984 R²   (0.964 R² .. 0.997 R²)
mean                 68.73 ms   (62.24 ms .. 71.68 ms)
std dev              7.389 ms   (3.628 ms .. 12.68 ms)
variance introduced by outliers: 35% (moderately inflated)

--------------------------------------------------------------------------------

***** benchmark input list: Descending, size: 10000  *****
benchmarking quicksort/Classic:
time                 8.972 s    (7.923 s .. 9.668 s)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 9.395 s    (9.076 s .. 9.519 s)
std dev              218.8 ms   (8.620 ms .. 286.2 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Diller:
time                 26.51 s    (21.91 s .. 29.73 s)
                     0.997 R²   (0.989 R² .. 1.000 R²)
mean                 26.00 s    (24.89 s .. 26.63 s)      -- outlier?
std dev              1.088 s    (310.7 ms .. 1.464 s)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Leal:
time                 1.173 s    (1.037 s .. 1.292 s)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 1.163 s    (1.150 s .. 1.187 s)
std dev              23.00 ms   (992.6 μs .. 28.28 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/LealM:
time                 1.082 s    (862.6 ms .. 1.178 s)
                     0.995 R²   (0.993 R² .. 1.000 R²)
mean                 1.158 s    (1.098 s .. 1.178 s)
std dev              39.95 ms   (405.7 μs .. 46.72 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Bird:
time                 1.087 s    (974.1 ms .. 1.182 s)
                     0.998 R²   (0.998 R² .. 1.000 R²)
mean                 1.131 s    (1.106 s .. 1.146 s)
std dev              25.03 ms   (10.81 ms .. 34.61 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort-split/split:
time                 369.1 μs   (366.0 μs .. 373.1 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 368.2 μs   (364.7 μs .. 371.3 μs)
std dev              10.87 μs   (8.592 μs .. 14.31 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking quicksort-split/split'':
time                 112.3 μs   (111.3 μs .. 113.6 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 112.7 μs   (111.2 μs .. 114.6 μs)
std dev              5.543 μs   (4.102 μs .. 7.911 μs)
variance introduced by outliers: 51% (severely inflated)

--------------------------------------------------------------------------------

***** benchmark input list: Ascending, size: 10000  *****
benchmarking quicksort/Classic:
time                 1.027 s    (958.3 ms .. 1.078 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.052 s    (1.033 s .. 1.060 s)
std dev              13.99 ms   (4.079 ms .. 19.00 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Diller:
time                 2.862 s    (2.622 s .. 3.084 s)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 3.008 s    (2.916 s .. 3.089 s)
std dev              101.1 ms   (48.92 ms .. 130.6 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Leal:
time                 1.178 s    (1.124 s .. 1.227 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.172 s    (1.153 s .. 1.179 s)
std dev              12.98 ms   (2.212 ms .. 16.82 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/LealM:
time                 1.131 s    (1.079 s .. 1.200 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.192 s    (1.162 s .. 1.217 s)
std dev              33.35 ms   (24.47 ms .. 39.32 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Bird:
time                 1.113 s    (1.071 s .. 1.170 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.138 s    (1.125 s .. 1.148 s)
std dev              14.01 ms   (1.782 ms .. 19.72 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort-split/split:
time                 624.2 μs   (617.9 μs .. 631.4 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 615.7 μs   (609.3 μs .. 621.4 μs)
std dev              20.81 μs   (16.66 μs .. 26.06 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking quicksort-split/split'':
time                 120.4 μs   (119.5 μs .. 121.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 120.6 μs   (119.6 μs .. 121.9 μs)
std dev              3.692 μs   (2.675 μs .. 5.048 μs)
variance introduced by outliers: 28% (moderately inflated)

--------------------------------------------------------------------------------

-- | No `BigDescending` benchmarks, as all implementations take very long time.
-- 1. In particular, `Classic` & `Diller` freeze up my computer (memory hog), 
--    while `Leal` & `Bird` don't, but they take far too long as well, so I 
--    didn't bother benchmarking them either.
-- 2. Just from this memory hog issue, I conclude that  `Leal` & `Bird` are far 
--    better than `Diller` & `Classic`, even for this case.

--------------------------------------------------------------------------------


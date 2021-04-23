-- sample benchmarks after creation of Benchmark.hs -- 23 APR 2021.
-- author: Prem Muthedath.
--------------------------------------------------------------------------------

***** benchmark input list: Simple, size: 10  *****
benchmarking quicksort/Classic:
time                 1.342 μs   (1.327 μs .. 1.358 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 1.337 μs   (1.325 μs .. 1.350 μs)
std dev              41.81 ns   (34.93 ns .. 53.19 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking quicksort/Diller:
time                 1.552 μs   (1.535 μs .. 1.570 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.570 μs   (1.555 μs .. 1.588 μs)
std dev              54.33 ns   (45.06 ns .. 66.78 ns)
variance introduced by outliers: 47% (moderately inflated)

benchmarking quicksort/Leal:
time                 695.2 ns   (688.7 ns .. 702.7 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 701.1 ns   (694.2 ns .. 709.2 ns)
std dev              24.61 ns   (19.99 ns .. 30.89 ns)
variance introduced by outliers: 50% (severely inflated)

benchmarking quicksort/LealM:
time                 721.9 ns   (716.2 ns .. 728.9 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 738.3 ns   (730.0 ns .. 749.7 ns)
std dev              33.59 ns   (25.77 ns .. 48.13 ns)
variance introduced by outliers: 63% (severely inflated)

benchmarking quicksort/Bird:
time                 680.7 ns   (676.3 ns .. 685.3 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 683.4 ns   (677.8 ns .. 689.5 ns)
std dev              18.97 ns   (15.43 ns .. 23.85 ns)
variance introduced by outliers: 38% (moderately inflated)

benchmarking quicksort-splits/split:
time                 485.3 ns   (481.0 ns .. 490.2 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 483.6 ns   (478.5 ns .. 488.6 ns)
std dev              17.70 ns   (13.69 ns .. 24.64 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking quicksort-splits/split'':
time                 107.1 ns   (105.8 ns .. 108.5 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 106.2 ns   (104.8 ns .. 107.4 ns)
std dev              3.957 ns   (3.122 ns .. 4.991 ns)
variance introduced by outliers: 57% (severely inflated)

--------------------------------------------------------------------------------

***** benchmark input list: Random, size: 1000000  *****
benchmarking quicksort/Classic:
time                 2.682 s    (2.536 s .. 2.791 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.870 s    (2.779 s .. 2.960 s)
std dev              113.9 ms   (61.25 ms .. 137.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Diller:
time                 3.109 s    (2.484 s .. NaN s)
                     0.995 R²   (0.988 R² .. 1.000 R²)
mean                 3.222 s    (3.129 s .. 3.283 s)
std dev              91.20 ms   (24.42 ms .. 119.9 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Leal:
time                 1.534 s    (1.341 s .. 1.730 s)
                     0.997 R²   (0.997 R² .. 1.000 R²)
mean                 1.614 s    (1.567 s .. 1.643 s)
std dev              48.09 ms   (23.04 ms .. 67.37 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/LealM:
time                 1.582 s    (1.188 s .. 1.855 s)
                     0.991 R²   (0.986 R² .. 1.000 R²)
mean                 1.718 s    (1.638 s .. 1.763 s)
std dev              77.13 ms   (19.38 ms .. 103.6 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Bird:
time                 1.473 s    (1.149 s .. 1.770 s)
                     0.991 R²   (0.991 R² .. 1.000 R²)
mean                 1.615 s    (1.538 s .. 1.663 s)
std dev              77.81 ms   (33.82 ms .. 108.3 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort-splits/split:
time                 100.2 ms   (93.67 ms .. 105.5 ms)
                     0.993 R²   (0.972 R² .. 0.999 R²)
mean                 91.95 ms   (80.34 ms .. 96.41 ms)
std dev              10.98 ms   (4.361 ms .. 17.00 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking quicksort-splits/split'':
time                 71.18 ms   (66.51 ms .. 74.29 ms)
                     0.992 R²   (0.979 R² .. 0.998 R²)
mean                 69.68 ms   (63.80 ms .. 72.64 ms)
std dev              7.346 ms   (3.494 ms .. 12.40 ms)
variance introduced by outliers: 35% (moderately inflated)

--------------------------------------------------------------------------------

***** benchmark input list: Descending, size: 10000  *****
benchmarking quicksort/Classic:
time                 11.73 s    (9.027 s .. 14.70 s)
                     0.992 R²   (0.972 R² .. 1.000 R²)
mean                 11.56 s    (11.14 s .. 12.04 s)
std dev              510.4 ms   (213.9 ms .. 710.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Diller:
time                 14.83 s    (12.99 s .. 17.33 s)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 14.57 s    (14.06 s .. 14.86 s)
std dev              492.8 ms   (150.0 ms .. 663.8 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Leal:
time                 1.397 s    (1.073 s .. 1.546 s)
                     0.994 R²   (0.984 R² .. 1.000 R²)
mean                 1.496 s    (1.441 s .. 1.525 s)
std dev              54.21 ms   (4.144 ms .. 65.86 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/LealM:
time                 1.395 s    (1.128 s .. 1.520 s)
                     0.995 R²   (0.990 R² .. NaN R²)
mean                 1.508 s    (1.425 s .. 1.540 s)
std dev              56.55 ms   (6.112 ms .. 72.01 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Bird:
time                 1.414 s    (1.149 s .. 1.536 s)
                     0.996 R²   (0.990 R² .. 1.000 R²)
mean                 1.502 s    (1.454 s .. 1.526 s)
std dev              46.73 ms   (1.331 ms .. 55.71 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort-splits/split:
time                 453.2 μs   (448.8 μs .. 457.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 458.6 μs   (453.8 μs .. 465.8 μs)
std dev              19.31 μs   (15.87 μs .. 24.79 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking quicksort-splits/split'':
time                 119.1 μs   (117.8 μs .. 120.4 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 119.6 μs   (118.0 μs .. 121.4 μs)
std dev              5.427 μs   (4.259 μs .. 7.132 μs)
variance introduced by outliers: 46% (moderately inflated)

--------------------------------------------------------------------------------

***** benchmark input list: Ascending, size: 10000  *****
benchmarking quicksort/Classic:
time                 1.783 s    (1.643 s .. 2.000 s)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 1.924 s    (1.852 s .. 1.978 s)
std dev              70.73 ms   (36.99 ms .. 87.67 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Diller:
time                 3.333 s    (3.244 s .. 3.400 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.571 s    (3.461 s .. 3.703 s)
std dev              151.5 ms   (8.660 ms .. 190.1 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Leal:
time                 1.396 s    (1.098 s .. 1.547 s)
                     0.994 R²   (0.988 R² .. 1.000 R²)
mean                 1.498 s    (1.442 s .. 1.526 s)
std dev              53.89 ms   (3.508 ms .. 66.24 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/LealM:
time                 1.522 s    (1.341 s .. 1.625 s)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 1.548 s    (1.519 s .. 1.564 s)
std dev              31.64 ms   (26.35 ms .. 34.03 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort/Bird:
time                 1.499 s    (1.349 s .. 1.618 s)
                     0.998 R²   (0.998 R² .. 1.000 R²)
mean                 1.538 s    (1.508 s .. 1.559 s)
std dev              30.48 ms   (15.64 ms .. 39.10 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking quicksort-splits/split:
time                 817.5 μs   (805.9 μs .. 829.7 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 816.5 μs   (808.7 μs .. 825.0 μs)
std dev              28.69 μs   (23.53 μs .. 35.41 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking quicksort-splits/split'':
time                 120.8 μs   (119.4 μs .. 122.1 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 118.7 μs   (117.0 μs .. 120.3 μs)
std dev              5.464 μs   (4.485 μs .. 6.669 μs)
variance introduced by outliers: 47% (moderately inflated)

--------------------------------------------------------------------------------

-- | No `BigDescending` benchmarks, as all implementations take very long time.
-- 1. In particular, `Classic` & `Diller` freeze up my computer (memory hog), 
--    while `Leal` & `Bird` don't, but they take far too long as well, so I 
--    didn't bother benchmarking them either.
-- 2. Just from this memory hog issue, I conclude that  `Leal` & `Bird` are far 
--    better than `Diller` & `Classic`, even for this case.

--------------------------------------------------------------------------------


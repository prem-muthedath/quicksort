#### quicksort

Benchmark haskell quicksort implementations -- classic,  Diller, Leal, Bird.

HOW TO RUN THE PROGRAM AND TESTS:
  1. `cd` to `quicksort` -- the top directory containing this `README` file.
  2. to run benchmarks, type below command and press `RETURN`:
        - `cabal v2-run :quicksort`
  3. to run all (QuickCheck) tests, type below command & press `RETURN`:
        - `cabal v2-run :quicksort-test`
  4. you can specify on the commandline the list type you want to use in
     QuickCheck tests.  Default is `[Int]`.  To see available options & usage, 
     type below command & press `RETURN`:
        - `cabal v2-run :quicksort-test -- --help`



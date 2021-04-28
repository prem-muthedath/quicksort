-- sample quickcheck test results. 28 APR 2021.
-- author: Prem Muthedath.

--- Classic ---
testing with: [Int]
*** property: ordered ***
+++ OK, passed 100 tests:
78% has duplicates
73% has > 10 elements
 9% pre-ordered
 4% empty
*** property: invariance ***
+++ OK, passed 100 tests:
67% has duplicates
64% has > 10 elements
12% pre-ordered
 6% empty
*** property: model sort equivalence ***
+++ OK, passed 100 tests:
66% has > 10 elements
66% has duplicates
 8% pre-ordered
 3% empty
*** property: minimum ***
+++ OK, passed 100 tests:
71% has > 10 elements
70% has duplicates
 5% pre-ordered
*** property: maximum ***
+++ OK, passed 100 tests:
74% has duplicates
68% has > 10 elements
11% pre-ordered

--- Diller ---
testing with: [Int]
*** property: ordered ***
+++ OK, passed 100 tests:
66% has duplicates
61% has > 10 elements
10% pre-ordered
 2% empty
*** property: invariance ***
+++ OK, passed 100 tests:
66% has > 10 elements
65% has duplicates
10% pre-ordered
 6% empty
*** property: model sort equivalence ***
+++ OK, passed 100 tests:
59% has duplicates
58% has > 10 elements
13% pre-ordered
 3% empty
*** property: minimum ***
+++ OK, passed 100 tests:
67% has duplicates
64% has > 10 elements
 5% pre-ordered
*** property: maximum ***
+++ OK, passed 100 tests:
67% has duplicates
65% has > 10 elements
 9% pre-ordered

--- Leal ---
testing with: [Int]
*** property: ordered ***
+++ OK, passed 100 tests:
68% has duplicates
60% has > 10 elements
12% pre-ordered
 3% empty
*** property: invariance ***
+++ OK, passed 100 tests:
65% has > 10 elements
65% has duplicates
14% pre-ordered
 5% empty
*** property: model sort equivalence ***
+++ OK, passed 100 tests:
69% has > 10 elements
63% has duplicates
 6% pre-ordered
 3% empty
*** property: minimum ***
+++ OK, passed 100 tests:
69% has > 10 elements
65% has duplicates
 4% pre-ordered
*** property: maximum ***
+++ OK, passed 100 tests:
65% has > 10 elements
65% has duplicates
 6% pre-ordered

--- LealM ---
testing with: [Int]
*** property: ordered ***
+++ OK, passed 100 tests:
68% has > 10 elements
65% has duplicates
14% pre-ordered
 8% empty
*** property: invariance ***
+++ OK, passed 100 tests:
75% has duplicates
67% has > 10 elements
10% pre-ordered
 2% empty
*** property: model sort equivalence ***
+++ OK, passed 100 tests:
66% has duplicates
64% has > 10 elements
10% pre-ordered
 3% empty
*** property: minimum ***
+++ OK, passed 100 tests:
71% has > 10 elements
71% has duplicates
 9% pre-ordered
*** property: maximum ***
+++ OK, passed 100 tests:
65% has > 10 elements
65% has duplicates
13% pre-ordered

--- Bird ---
testing with: [Int]
*** property: ordered ***
+++ OK, passed 100 tests:
70% has duplicates
69% has > 10 elements
11% pre-ordered
 3% empty
*** property: invariance ***
+++ OK, passed 100 tests:
65% has > 10 elements
63% has duplicates
12% pre-ordered
 5% empty
*** property: model sort equivalence ***
+++ OK, passed 100 tests:
67% has duplicates
59% has > 10 elements
11% pre-ordered
 4% empty
*** property: minimum ***
+++ OK, passed 100 tests:
72% has duplicates
69% has > 10 elements
 5% pre-ordered
*** property: maximum ***
+++ OK, passed 100 tests:
74% has duplicates
70% has > 10 elements
 9% pre-ordered


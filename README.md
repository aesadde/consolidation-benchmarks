# Consolidation Benchmarks

This repo provides all the benchmarks used for my masters thesis "Consolidation
of Haskell programs".

It contains a set of 12 (f1_all and m2_all contain 2 tests each) benchmarks:
  * m1_addone
  * m1_cub
  * m2_all
  * f1_all
  * f2_lists
  * f2_vectors
  * f3_simple
  * f3_complex
  * f4_sum
  * f4_tuple

## Running instructions

* Clone or download this repo

* Install [stack](https://docs.haskellstack.org/en/stable/README/).

* Run `stack build`. This should install all the necessary packages, including the modified version
  of the `vector` package included in this project.

  The modification of `vector` is only needed for `f4_sum` and `f4_tuple` since the function
  we implemented, depends on functions that are not exposed in the original version.
  No core functionality is modified from the original implementation.

* Run `stack exec cons-bench` and select the number of iterations that you want to perform.
  This will run all the benchmarks.

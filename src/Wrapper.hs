{-|
Module      : Wrapper
Description : Wrapper to run all weighs together
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module Wrapper (bench_all) where

import  Utils
import  Weigh
import MapR1
import MapR2
import FilterR1
import FilterR2
import FilterR3
import FilterR4
import FoldR1

prettyp rule = do
  putStrLn "-----------------------------------------------------------------"
  putStrLn $ "---------------------- " ++ rule ++ " -----------------------------------"

bench_all iters = do
  putStrLn $ "Running all the benchmarks with " ++ (show iters) ++ " iterations"
  prettyp "Map R1"
  m1_addone True iters
  m1_cube True iters
  prettyp "Map R2"
  m2_all True iters
  prettyp "Filter R1"
  f1_all True iters
  prettyp "Filter R2"
  f2_lists True iters
  f2_vectors True iters
  prettyp "Filter R3"
  f3_simple True iters
  f3_complex True iters
  prettyp "Filter R4"
  f4_sum True iters
  f4_tuple True iters
  prettyp "Fold R1"
  fd1_sum True iters
  fd1_mult True iters
  putStrLn "------------------FINISHED -----------------------------------"

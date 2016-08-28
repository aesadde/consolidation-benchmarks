{-# LANGUAGE BangPatterns #-}

{-|
Module      : MapR2
Description : Benchmarks for the rule Map R2
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module  MapR2 (m2_all) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils
-------------------------------------------------------------------------------
-- Benchmark functions --------------------------------------------------------
-------------------------------------------------------------------------------
maps_r2 iters f g rand bench msg genRand genFun = do
  let opt = mapr2_opt f g
      orig = mapr2 f g
  bench_single_prep opt orig rand bench msg genRand genFun iters

mapr2 f g xs =  let !f1 = V.map f xs
                    !f2 = V.map g xs
                in (f1,f2)

mapr2_opt f g xs = V.foldr (\x (m1,m2) ->
                   (V.cons (f x) m1, V.cons (g x) m2)) (V.empty,V.empty) xs

-------------------------------------------------------------------------------
-- Benchmarks -----------------------------------------------------------------
-------------------------------------------------------------------------------
m2_all bench iters = do
  let f = (\x-> x^3)
      g = (\x -> x `mod` 5 == 0)
      h = (+1)
      i = (*2)
      opt = mapr2_opt f g
      orig = mapr2 f g
      opt1 = mapr2_opt h i
      orig1 = mapr2 h i
      m1  = "Map R2 Sequence Mod Operations "
      m2  = "Map R2 Sequence Integer Comparison"
      m3  = "Map R2 Random Mod Operations"
      m4  = "Map R2 Random Integer Comparison"
  t1 <- bench_single_prep opt orig False bench m1 getRandVec argvector iters
  t2 <- bench_single_prep opt1 orig1 False bench m2 getRandVec argvector iters
  t3 <- bench_single_prep opt orig True bench m3 getRandVec argvector iters
  t4 <- bench_single_prep opt1 orig1 True bench m4 getRandVec argvector iters
  run t1 t2 t3 t4 bench

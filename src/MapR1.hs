{-# LANGUAGE BangPatterns #-}

{-|
Module      : MapR1
Description : Benchmarks for the rule Map R1
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module  MapR1 (m1_addone, m1_cube) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils
-------------------------------------------------------------------------------
-- Benchmark functions --------------------------------------------------------
-------------------------------------------------------------------------------
maps f xs ys = (V.map f xs, V.map f ys)

maps_opt :: (a -> a1) -> Vector a2 -> Vector a -> (Vector a1, Vector a1)
maps_opt f xs ys = let l = V.map f ys
                   in (V.take (V.length xs) l, l)

m1_all iters f bench = do
  let opt  = (maps_opt f)
      orig = (maps f)
      m1   = "Map R1 Sequence EqualSize"
      m2   = "Map R1 Sequence HalfSize"
      m3   = "Map R1 Random EqualSize"
      m4   = "Map R1 Random HalfSize"
  t1 <- bench_double_prep opt orig False bench m1 1 getRandVec argvector iters
  t2 <- bench_double_prep opt orig False bench m2 2 getRandVec argvector iters
  t3 <- bench_double_prep opt orig True bench m3 1 getRandVec argvector iters
  t4 <- bench_double_prep opt orig True bench m4 2 getRandVec argvector iters
  run t1 t2 t3 t4 bench

m1_addone :: Integral b => Bool -> b -> IO ()
m1_addone bench iters = m1_all iters (+) bench

m1_cube :: Integral b => Bool -> b -> IO ()
m1_cube bench iters   = m1_all iters (\x -> x^3) bench

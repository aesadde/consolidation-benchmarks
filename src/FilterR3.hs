{-# LANGUAGE BangPatterns #-}

{-|
Module      :  FilterR3
Description : Benchmarks for rule Filter R3
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module  FilterR3 (f3_simple, f3_complex) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils

-------------------------------------------------------------------------------
-- Benchmark functions --------------------------------------------------------
-------------------------------------------------------------------------------
filter3 :: (a -> Bool) -> Vector a -> Vector a -> (Vector a, Vector a)
filter3 p xs ys = let !f1 = V.filter p xs
                      !f2 =  V.filter p ys
                   in (f1,f2)

filter3_opt :: (a -> Bool) -> Vector a1 -> Vector a -> (Vector a, Vector a)
filter3_opt p xs ys = let !f1 = V.filter p ys
                          !f2 = f1 V.++ V.filter p (V.drop (V.length xs) f1)
                       in (f1, f2)
-------------------------------------------------------------------------------
-- Benchmarks -----------------------------------------------------------------
-------------------------------------------------------------------------------
f3_all p m bench iters = do
  let opt  = (filter3_opt p)
      orig = (filter3 p)
      m1   = "Filter R3 Sequence EqualSize -- " ++ m
      m2   = "Filter R3 Sequence HalfSize -- " ++ m
      m3   = "Filter R3 Random EqualSize -- " ++ m
      m4   = "Filter R3 Random HalfSize -- " ++ m
  t1 <- bench_double_prep opt orig False bench m1 1 getRandVec argvector iters
  t2 <- bench_double_prep opt orig False bench m2 2 getRandVec argvector iters
  t3 <- bench_double_prep opt orig True bench m3 1 getRandVec argvector iters
  t4 <- bench_double_prep opt orig True bench m4 2 getRandVec argvector iters
  run t1 t2 t3 t4 bench


f3_simple :: Integral b => Bool -> b -> IO ()
f3_simple bench iters = f3_all (even) "Simple Operations" bench iters

f3_complex :: Integral b => Bool -> b -> IO ()
f3_complex bench iters =
  f3_all (\x -> x*3 `mod` 5 == 0) "Complex Operations" bench iters

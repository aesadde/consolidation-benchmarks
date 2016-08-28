{-# LANGUAGE BangPatterns #-}
{-|
Module      :  FilterR4
Description : Benchmarks for rule Filter R4
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module FilterR4 (f4_sum, f4_tuple) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils

-------------------------------------------------------------------------------
-- Benchmark Functions --------------------------------------------------------
-------------------------------------------------------------------------------

filter4 :: (a -> Bool) -> (a -> Bool) ->
            (Vector a -> Vector a -> t) -> Vector a -> t
filter4 p1 p2 g xs = let !ys = V.filter p1 xs
                         !zs = V.filter p2 ys
                     in g ys zs

filter4_opt :: (a -> Bool) -> (a -> Bool) ->
                (Vector a -> Vector a -> t) -> Vector a -> t
filter4_opt p1 p2 g xs = let !(ys,zs) = V.myFilter p1 p2 xs
                         in g ys zs

-------------------------------------------------------------------------------
-- Benchmarks -----------------------------------------------------------------
--------------------------------------------------------------------------------

f4_msg :: [Char] -> [Char] -> [Char] -> [Char]
f4_msg r p t = "Filter R4 " ++ r ++ " " ++ p ++ " " ++ t

f4_bench opt orig rand bench msg iters =
  bench_single_prep opt orig rand bench msg getRandVec argvector iters

f4_all g gs bench iters = do
  let p1    = (even)
      p2    = (\x -> x `mod` 3 == 0)
      p3    = (\x -> x*2 `mod` 5 == 0)
      ps = "Simple Preds"
      cs = "Complex Preds"
      opt   = filter4_opt p1 p2 g
      orig  = filter4 p1 p2 g
      opt1  = filter4_opt p2 p3 g
      orig1 = filter4 p2 p3 g
  t1 <- f4_bench opt orig False bench (f4_msg "Sequence" ps gs) iters
  t2 <- f4_bench opt1 orig1 False bench (f4_msg "Sequence" cs gs) iters
  t3 <- f4_bench opt orig True bench (f4_msg "Random" ps gs) iters
  t4 <- f4_bench opt1 orig1 True bench (f4_msg "Random" cs gs) iters
  run t1 t2 t3 t4 bench

sums :: Vector Integer -> Vector Integer -> Integer
sums = (\x y -> sum x + sum y)

f4_sum :: Integral b => Bool -> b -> IO ()
f4_sum bench iters = f4_all sums "Sum" bench iters

f4_tuple :: Integral b => Bool -> b -> IO ()
f4_tuple bench iters = f4_all (,) "Tuples" bench iters

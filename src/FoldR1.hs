{-# LANGUAGE BangPatterns #-}

{-
Module      :  FoldR1
Description : Benchmarks for rule Fold R1
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module FoldR1 (fd1_sum, fd1_mult) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils
-------------------------------------------------------------------------------
-- Benchmark functions --------------------------------------------------------
-------------------------------------------------------------------------------

fold1 :: (a -> t -> t) -> t -> Vector a -> Vector a -> (t, t)
fold1 f acc xs ys =  let !f1 = V.foldr f acc xs
                         !f2 = V.foldr f acc ys
                       in (f1,f2)

fold1_opt :: (a -> t -> t) -> t -> Vector a -> Vector a -> (t, t)
fold1_opt f e xs ys = let acc = V.foldr f e xs
                       in (acc, V.foldr f acc (V.drop (V.length xs) ys))

fd1_bench opt orig rand bench msg split iters =
  bench_double_prep opt orig rand bench msg split getRandVec argvector iters

fd1_all p e bench iters = do
  let opt  = fold1_opt p e
      orig = fold1 p e
      m1   = "Fold R1 Sequence EqualSize"
      m2   = "Fold R1 Sequence HalfSize"
      m3   = "Fold R1 Random EqualSize"
      m4   = "Fold R1 Random HalfSize"
  t1 <- fd1_bench opt orig False bench m1 1 iters
  t2 <- fd1_bench opt orig False bench m2 2 iters
  t3 <- fd1_bench opt orig True bench m3 1 iters
  t4 <- fd1_bench opt orig True bench m4 2 iters
  run t1 t2 t3 t4 bench

fd1_sum :: Integral b => Bool -> b -> IO ()
fd1_sum bench iters = fd1_all (+) 0 bench iters

fd1_mult :: Integral b => Bool -> b -> IO ()
fd1_mult bench iters = fd1_all (\x y -> x*2 + y) 0 bench iters

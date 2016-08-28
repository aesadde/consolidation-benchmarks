{-# LANGUAGE BangPatterns #-}

{-|
Module      :  FilterR2
Description : Benchmarks for rule Filter R2
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module  FilterR2 (f2_lists, f2_vectors) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils

-------------------------------------------------------------------------------
-- Benchmark functions --------------------------------------------------------
-------------------------------------------------------------------------------
filter2 :: (a -> Bool) -> (a -> Bool) -> Vector a -> (Vector a, Vector a)
filter2 p q xs = let !f1 = V.filter p xs
                     !f2 = V.filter q xs
                  in (f1,f2)

filter2_opt :: (a -> Bool) -> (a -> Bool) -> Vector a -> (Vector a, Vector a)
filter2_opt p q xs = let !(f1,f2) = twoFilters p q xs
                      in (f1, f2)
  where
    twoFilters p q xs = V.foldr (select p q) (V.empty, V.empty) xs
    select p q x (a,b)
      | p x = (V.cons x a, V.cons x b)
      | otherwise = (a, if q x then V.cons x b else b)

-- Lists ----------------------------------------------------------------------
filter2l :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
filter2l p q xs = let !f1 = filter p xs
                      !f2 = filter q xs
                  in (f1,f2)

filter2_optl :: Foldable t => (a -> Bool) -> (a -> Bool) -> t a -> ([a], [a])
filter2_optl p q xs = let !(f1,f2) = twoFilters p q xs
                      in (f1, f2)
  where
    twoFilters p q xs = foldr (select p q) ([],[]) xs
    select p q x (a,b)
      | p x = (x:a, x:b)
      | otherwise = (a, if q x then x:b else b)

-------------------------------------------------------------------------------
-- Benchmarks -----------------------------------------------------------------
-------------------------------------------------------------------------------
msg_simp :: [Char] -> [Char] -> [Char]
msg_simp l rand = "Filter R2 -- " ++ l ++ " -- Simple Operations -- " ++ rand

msg_comp :: [Char] -> [Char] -> [Char]
msg_comp l rand = "Filter R2 -- " ++ l ++ " -- Complex Operations -- " ++ rand

f2_all f2 f2_opt l getRand genFun iters bench =  do
  let p     = (\x -> x `mod` 4 == 0)
      q     = (even)
      p1    = (\x -> x*100 `mod` 4 == 0)
      q1    = (\x -> even (x*100))
      opt   = f2_opt p q
      orig  = f2 p q
      opt1  = f2_opt p1 q1
      orig1 = f2 p1 q1
      m1    = msg_simp l "Sequence"
      m2    = msg_comp l "Sequence"
      m3    = msg_simp l "Random"
      m4    = msg_comp l "Random"
  t1 <- bench_single_prep opt orig True bench m1 getRand genFun iters
  t2 <- bench_single_prep opt1 orig1 True bench m2 getRand genFun iters
  t3 <- bench_single_prep opt orig False bench m3 getRand genFun iters
  t4 <- bench_single_prep opt1  orig1 False bench m4 getRand genFun iters
  run t1 t2 t3 t4 bench

f2_lists :: Integral b => Bool -> b -> IO ()
f2_lists bench iters      =
  f2_all filter2l filter2_optl"Lists" getRandList arglist iters bench

f2_vectors :: Integral b => Bool -> b -> IO ()
f2_vectors bench  iters   =
  f2_all filter2 filter2_opt "Vectors" getRandVec argvector iters bench

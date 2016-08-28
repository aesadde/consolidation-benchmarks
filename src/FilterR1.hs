{-# LANGUAGE BangPatterns #-}

{-|
Module      :  FilterR1
Description : Benchmarks for the rule Filter R1
Copyright   : (c) Alberto, 2016
License     : MIT
Maintainer  : albertosadde@gmail.com
Stability   : experimental
-}

module  FilterR1 (f1_all) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import           Utils

-------------------------------------------------------------------------------
-- Benchmark functions ---------------------------------------------------------
-------------------------------------------------------------------------------

filter1 :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
filter1 p vs = let !f1 = V.filter p vs
                   !f2 = V.filter (not . p) vs
                in (f1,f2)

filter1_opt :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
filter1_opt p vs = let !(f1,f2) = V.partition p vs
                   in (f1,f2)

-------------------------------------------------------------------------------
-- Benchmarks -----------------------------------------------------------------
-------------------------------------------------------------------------------
msg_simp :: [Char] -> [Char]
msg_simp rand = "Filter R1 -- Split in Half -- " ++ rand

msg_comp :: [Char] -> [Char]
msg_comp  rand = "Filter R1 -- Mod Operations -- " ++ rand

gen_funs :: t2 -> (t, t2 -> t1) -> (t, t2 -> t1) -> [(t, t1)]
gen_funs p (n1,p1) (n2,p2) = [(n1, p1 p) , (n2, p2 p)]

gen_parts :: (a -> Bool) -> [([Char], Vector a -> (Vector a, Vector a))]
gen_parts p = gen_funs p ("fiter1_opt",filter1_opt) ("filter1", filter1)

f1_all bench iters = do
  let m1 = msg_simp "Sequence"
      m2 = msg_comp "Sequence"
      m3 = msg_simp "Random"
      m4 = msg_comp "Random"
      p = (\x -> x `mod` 5 == 0)
      opt = filter1_opt p
      orig = filter1 p
  t1 <- bench_multi_prep gen_parts False bench m1 getRandVec argvector iters
  t2 <- bench_single_prep opt orig False bench m2 getRandVec argvector iters
  t3 <- bench_multi_prep gen_parts True bench m3 getRandVec argvector iters
  t4 <- bench_single_prep opt orig True bench m4 getRandVec argvector iters
  run t1 t2 t3 t4 bench

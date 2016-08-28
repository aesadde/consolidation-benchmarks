module Main where

import Wrapper

main :: IO ()
main = do
  putStrLn "Consolidation Benchmarks"
  putStrLn "You can choose the number of iterations\n We start with vectors of size 100 and each iteration 10xs the size"
  putStrLn "Select the number of iterations: "
  args <- getLine
  let iters = read args :: Int
  bench_all iters

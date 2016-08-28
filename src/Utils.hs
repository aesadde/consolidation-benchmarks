{-# LANGUAGE BangPatterns #-}
module Utils where

import System.Random
import           Data.Map (Map)
import qualified Data.Map as  M
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList,empty)
import Criterion.Main
import Control.Monad (forM,forM_,mapM)
import           Data.Either.Utils (fromLeft,fromRight)
import           Control.Applicative
import Weigh

-------------------------------------------------------------------------------
-- Generate Data --------------------------------------------------------------
-------------------------------------------------------------------------------
-- | Generate a list from 1 to n
arglist :: Integer -> [Integer]
arglist n = [1..n]

-- | Generate a list from m to n
arglist' _ 0 = []
arglist' m n = [m..n]

-- | Generate a vector from 1 to n
argvector :: Integer -> Vector Integer
argector 0 = V.empty
argvector n = V.fromList [1..n]

-- | Generate a List of random elements of size n
getRandList :: RandomGen g => g -> Integer -> [Integer]
getRandList gen 0 = []
getRandList gen n = v:rest
  where
    (v, gen') = random gen
    rest = getRandList gen' (n - 1)

-- | Generate a list of random strings of size n
getRandStrings :: RandomGen g => g -> Int -> Integer -> [String]
getRandStrings  _ _  0 = []
getRandStrings gen s n = v:rest
  where
    v = take s $ randomRs ('a', 'z') gen
    (_,gen') = randomR ('a','z') gen
    rest = getRandStrings gen' s (n  -1)

-- | Generate a vector of of random elements of size n
getRandVec :: RandomGen g => g -> Integer -> Vector Integer
getRandVec gen 0    = V.empty
getRandVec gen n    = V.fromList (getRandList gen n)

-------------------------------------------------------------------------------
-- Set up Benchmark Env -------------------------------------------------------
-------------------------------------------------------------------------------
-- | Generate the input data with the given function
setupEnv :: Monad m => t -> (t -> r) -> m r
setupEnv n genFunc = return $ genFunc n

-- | Generates two lists for the given benchmark
setupEnv' :: Monad m => (t2 -> t) -> (t2 -> t1) -> t2 -> m (t, t1)
setupEnv' getFun1 getFun2 n = return (getFun1 n, getFun2 n)

-- | Same as @setupEnv' but generated data has different sizes
setupEnv'' :: Monad m => (t1 -> t) -> t1 -> t1 -> m (t, t)
setupEnv'' genFunc m n = return (genFunc m, genFunc n)

-------------------------------------------------------------------------------
-- Set up Benchmarks ----------------------------------------------------------
-------------------------------------------------------------------------------
benchFuns whnf_f whnf_g msg genFunc n = do
  env (setupEnv n genFunc) $ \ ~elems -> bgroup msg [
      bench "opt" $ nf whnf_f elems
    , bench "normal" $ nf whnf_g elems
    ]

benchFuns'' whnf_f whnf_g msg genFunc m n = do
  env (setupEnv'' genFunc m n) $ \ ~(xs,ys) -> bgroup msg [
      bench "opt"     $ nf (whnf_f xs) ys
    , bench "normal"  $ nf (whnf_g xs) ys
    ]

-- Same as benchFuns but takes a list of whnfs instead of only two
benchFuns' msg genFunc n whnfs = do
  env (setupEnv n genFunc) $ \ ~elems ->
    bgroup msg $ map (\(m,wh) -> bench (gen_msg m n) $ nf wh elems) whnfs

-- runs the benchmarks where times specifies the number of iterations
-- and the size of each vector
benchFull whnf_f whnf_g msg genFunc times = do
  let benches = map (\t ->
        benchFuns whnf_f whnf_g (gen_msg msg t) genFunc t) times
  defaultMain benches

gen_msg msg n = msg ++ " -- elems " ++ show n

gen_msg' msg n f = msg ++ " -- elems " ++ show n ++ f

-- Space usage ----------------------------------------------------------------
weigh_group msg f g vs = do
  mapM (\v ->
    do
        func (gen_msg' msg (length v) "opt") f v
        func (gen_msg' msg (length v) "normal") g v) vs

-- Weigh (t ())
weigh_group' msg f g vs = do
  mapM (\(m,v) -> do
          func (gen_msg' msg (length v) " opt") (f m) v
          func (gen_msg' msg (length v) " normal") (g m) v) vs

-- Weigh (t1 ())
weigh_groups msg genFunc n whnfs = do
  let elems = genFunc n
  mapM (\(m,wh) -> func (gen_msg' msg n m) wh elems) whnfs

weigh msg f g genFunc iters = do
  let !vs = map genFunc iters
  mainWith (weigh_group msg f g vs)

-- Utilities to run benchmarks in bulk

-- Prepares benchmarks for 2 functions that take a single list as input
bench_single_prep opt orig rand bench msg genRand genFun iters = do
  gen <- getStdGen
  let genFunc = if rand
                then genRand gen
                else genFun
      !times = [10^t | t <- [2..iters]]
  if bench
  then return . Left $ map (\t -> benchFuns opt orig (gen_msg msg t) genFunc t) times
  else return .  Right  $ weigh_group msg opt orig (map genFunc times)

-- Prepares a set of benchmarks that receive 2 lists as input
-- IO (Either [Benchmark] (Weigh [()]))
bench_double_prep opt orig rand bench msg split genRand genFun iters = do
  gen <- getStdGen
  let genFunc = if rand
                then (genRand gen)
                else genFun
      !times = [10^t | t <- [2..iters]]
      !xss   = map (\x -> x `div` split) times
      !total = zip xss times
  if bench
  then
    return . Left  $
      map (\(m,n) -> (benchFuns'' opt orig (gen_msg msg n) genFunc m n)) total
  else do
    lsts <- mapM (uncurry (setupEnv'' genFunc)) total
    let w = weigh_group' msg opt orig lsts
    return (Right w)

bench_multi_prep parts rand bench msg genRand genFun iters = do
  gen <- getStdGen
  let genFunc = if rand
                then genRand gen
                else genFun
      p n = (\x -> x >= n)
      times = [10^t | t <- [2..iters]] -- elems
      pns = [p (5*(10^x)) | x <- [1..iters]]
      whnfs = zip times (map parts pns) -- list of lists of whnfs
  if bench
  then do
    let bgroups = map (\(t,wh) -> benchFuns' (gen_msg msg t) genFunc t wh) whnfs
    return (Left bgroups)
  else do
      let weighs = concat <$> mapM (uncurry (weigh_groups msg genFunc)) whnfs
      return (Right weighs)

run t1 t2 t3 t4 bench
  | bench = defaultMain $ concatMap fromLeft ([t1] ++ [t2] ++ [t3] ++ [t4])
  | otherwise =
    do
      let b1 = fromRight t1
          b2   = fromRight t2
          b3   = fromRight t3
          b4   = fromRight t4
          bs   = b1 >>= (\x -> b2 >>= (\y -> return (x ++ y )))
          bs1  = b3 >>= (\x -> b4 >>= (\y -> return (x ++ y )))
          bss  = bs >>= (\x -> bs1 >>= (\y -> return (x ++ y)))
      mainWith bss

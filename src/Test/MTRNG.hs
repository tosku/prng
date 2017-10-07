module Test.MTRNG where

import qualified Data.List as L
import Data.IntMap
import qualified Data.Vector                   as V

import Test.Test
import qualified Data.PRNG as RNG
import qualified Data.PRNG.MTRNG as MT

fastTests :: [Test]
fastTests = [ test1
            , test2
            , test3
            , test4
            ]

test1 :: Test
test1 = do
  let name = "Checking MT uniform Double generation"
      n   = 1000000
      seed = 136
      mtrng = RNG.getRNG :: Int -> MT.MTRNG
      ts = RNG.uniformDoubles (mtrng seed) n 
      ts' r n
        | n ==  0 = []
        | otherwise =  (d,g') : ts' g' (n-1)
            where (d,g') = RNG.randomDouble r
  case ts == L.map fst (ts' (mtrng seed) n)  of
    True -> testPassed name $ show (sum ts) ++ show "passed!"
    False -> testFailed name $ (,) (show "problem with generation") (show "sorry")

test2 :: Test
test2 = do
  let name = "Check permutations contains all elements"
      n   = 10000
      seed = 139
      mtrng = RNG.getRNG :: Int -> MT.MTRNG
      ts = RNG.randomPermutation (mtrng seed) [1..n]
      out  = L.map (\ i -> elem i ts) [1..n]
  case all (== True) out of
    True -> testPassed name $ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")

test3 :: Test
test3 = do
  let name = "Check sampling"
      n   = 1000000
      seed = 135
      mtrng = RNG.getRNG :: Int -> MT.MTRNG
      x = 4
      ls = [1..n]
      sample = RNG.sample (mtrng seed) x ls
      out  = L.map (\ i -> elem i ls) sample
  case all (== True) out  && length sample == x of
    True -> testPassed name $ show sample ++ show "passed!"
    False -> testFailed name $ (,) (show sample) (show "sorry")

test4 :: Test
test4 = do
  let name = "Checking MT normal distribution"
      n   = 10000
      seed = 133
      μ = 4
      σ = 20
      mtrng = RNG.getRNG :: Int -> MT.MTRNG
      ns = RNG.normalDoubles (mtrng seed) μ σ n 
      us = RNG.uniformDoubles (mtrng seed) n 
  case ns /= us  of
    True -> testPassed name $ show ((sum ns) / fromIntegral n) ++ show "passed!"
    False -> testFailed name $ (,) (show "normal == uniform") (show "sorry")


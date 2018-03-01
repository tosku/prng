module Test.MWC where

import qualified Data.List as L
import Data.IntMap
import qualified Data.Vector                   as V

import TestHS
import qualified Data.PRNG as RNG
import qualified Data.PRNG.MWC as MWC

fastTests :: [Test]
fastTests = [ test1
            , test2
            , test3
            , test4
            ]

test1 :: Test
test1 = do
  let name = "Checking MWC uniform Double generation"
      n   = 1000
      seed = 136
      mwc = RNG.getRNG :: Int -> MWC.MWCRNG
      ts = RNG.uniformSample (mwc seed) n 
      ts' r n
        | n ==  0 = []
        | otherwise =  (d,g') : ts' g' (n-1)
            where (d,g') = RNG.randomDouble r
  case ts == L.map fst (ts' (mwc seed) n)  of
    True -> testPassed name $ show (sum ts) ++ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")

test2 :: Test
test2 = do
  let name = "Check permutations contains all elements"
      n   = 1000
      seed = 139
      mwcrng = RNG.getRNG :: Int -> MWC.MWCRNG
      ts = RNG.randomPermutation (mwcrng seed) [1..n]
      out  = L.map (\ i -> elem i ts) [1..n]
  case all (== True) out of
    True -> testPassed name $ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")

test3 :: Test
test3 = do
  let name = "Check sampling"
      n   = 1000
      seed = 135
      mwcrng = RNG.getRNG :: Int -> MWC.MWCRNG
      x = 4
      ls = [1..n]
      sample = RNG.sample (mwcrng seed) x ls
      out  = L.map (\ i -> elem i ls) sample
  case all (== True) out  && length sample == x of
    True -> testPassed name $ show sample ++ show "passed!"
    False -> testFailed name $ (,) (show sample) (show "sorry")

test4 :: Test
test4 = do
  let name = "Checking MWC normal distribution"
      n   = 1000
      seed = 131
      μ = 4
      σ = 20
      mwcrng = RNG.getRNG :: Int -> MWC.MWCRNG
      ns = RNG.normalSample (mwcrng seed) μ σ n 
      us = RNG.uniformSample (mwcrng seed) n 
  case ns /= us  of
    True -> testPassed name $ show ((sum ns) / fromIntegral n) ++ show "passed!"
    False -> testFailed name $ (,) (show "normal == uniform") (show "sorry")


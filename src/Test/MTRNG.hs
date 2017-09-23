module Test.MTRNG where

import qualified Data.List as L
import Data.IntMap
import qualified Data.Vector                   as V

import Test.Test
import Data.MTRNG

fastTests :: [Test]
fastTests = [ test1
            , test2
            , test3
            ]

test1 :: Test
test1 = do
  let name = "Check random Double generation"
      n   = 1000000000
      seed = 139
      ts = randomDoubles n seed
  case length ts == n of
    True -> testPassed name $ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")

test2 :: Test
test2 = do
  let name = "Check permutations contains all elements"
      n   = 10000
      seed = 139
      {-out  = all (L.map (\ i -> elem i (uniqueRandomInts n seed)) [1..n])-}
      ts = randomPermutation [1..n] seed
      out  = L.map (\ i -> elem i ts) [1..n]
  case all (== True) out of
    True -> testPassed name $ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")

test3 :: Test
test3 = do
  let name = "Check sampling"
      n   = 10000
      seed = 139
      x = 40
      {-out  = all (L.map (\ i -> elem i (uniqueRandomInts n seed)) [1..n])-}
      out = sample [1..n] x seed
  case length out == 40 of
    True -> testPassed name $ show out ++ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")

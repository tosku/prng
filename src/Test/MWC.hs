module Test.MWC where

import qualified Data.List as L
import Data.IntMap
import qualified Data.Vector                   as V

import Test.Test
import qualified Data.PRNG as RNG
import qualified Data.PRNG.MWC as MWC

fastTests :: [Test]
fastTests = [ test1
            ]

test1 :: Test
test1 = do
  let name = "Checking MWC random Double generation"
      n   = 1000000
      seed = 136
      mwc = RNG.getRNG :: Int -> MWC.MWCRNG
      ts = RNG.randomDoubles (mwc seed) n 
      ts' r n
        | n ==  0 = []
        | otherwise =  (d,g') : ts' g' (n-1)
            where (d,g') = RNG.randomDouble r
  case ts == L.map fst (ts' (mwc seed) n)  of
    True -> testPassed name $ show (sum ts) ++ show "passed!"
    False -> testFailed name $ (,) (show "not found all") (show "sorry")


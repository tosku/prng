module Main where

import Criterion.Main
import Data.MTRNG

-- Our benchmark harness.
main = do
  let rds = randomDoubles 
      n = 100000000
      s = 13
  writeFile "test.out" $ show $ sum $ rds n s 
  defaultMain [
    bgroup "mtrng" [ bench "1" $ whnf (randomDoubles n) s
                   , bench "2" $ whnf (\s -> randomBools n s 0.5) s
                   ]
    ]

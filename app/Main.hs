module Main where

import Criterion.Main
import qualified Data.PRNG as RNG
import qualified Data.PRNG.MTRNG as MT
{-import qualified Data.PRNG.ACMWC as ACMWC-}
import qualified Data.PRNG.MWC as MWC
import System.Random.MWC
import Control.Monad.Primitive
import qualified Data.Vector as V






-- Our benchmark harness.
main = do
  let n = 40000
      n2 = 2 * n
      s = 114
      mtrng = RNG.getRNG :: Int -> MT.MTRNG
      mwcrng = RNG.getRNG :: Int -> MWC.MWCRNG
      {-acmwcrng = RNG.getRNG :: Int -> ACMWC.MWCRNG-}
  writeFile "test1.out" $ 
    (show $ RNG.normalDoubles (mtrng s) 2.4 1.2 n )
  {-writeFile "test2.out" $ -}
    {-(show $ RNG.normalDoubles (mtrng s) 2.4 10.3 n )-}
  {-writeFile "test3.out" $ -}
    {-(show $ RNG.normalDoubles (mwcrng s) 2.4 10.3 n )-}
    {-(show $ sum $ RNG.uniformDoubles (mtrng s) n )-}
    {-++ "\n"-}
    {-++ (show $ sum $ RNG.uniformDoubles (mwcrng s) n )-}
    {-++ "\n"-}
    {-++ (show $ sum $ RNG.uniformDoubles (mwcrng s) (2*n)) -}
    {-++ "\n"-}
    {-++ (show $ sum $ RNG.randomDoubles (acmwcrng s) n )-}
  defaultMain [ 
                {-bgroup "mtrng" [ bench "10^6" $ whnf (\s -> sum $ RNG.randomDoubles (mtrng s) n) s-}
                               {-, bench "40^6" $ whnf (\s -> sum $ RNG.randomDoubles (mtrng s) (4 * n)) s-}
                               {-, bench "10^7" $ whnf (\s -> sum $ RNG.randomDoubles (mtrng s) (10 * n)) s-}
                               {-, bench "getRNG" $ whnf (\s -> RNG.randomDouble (mtrng s)) s-}
                               {-, bench "getRNG" $ whnf (\s -> RNG.randomDouble (mtrng s)) s-}
                    {-]-}
               bgroup "MWC-sampling" [ bench "1000" $ whnf (\s -> RNG.sample (mwcrng s) 100 [1..1000]) s
                                , bench "10^6" $ whnf (\s -> RNG.sample (mwcrng s) 100 [1..n]) s
                                , bench "normal distribution 10^6" $ whnf (\s -> RNG.normalDoubles (mwcrng s) 2.3 3.2 n) s
                    ]
              , bgroup "MT-sampling" [ bench "1000" $ whnf (\s -> RNG.sample (mtrng s) 100 [1..1000]) s
                                , bench "10^6" $ whnf (\s -> RNG.sample (mtrng s) 100 [1..n]) s
                                , bench "normal distribution 10^6" $ whnf (\s -> RNG.normalDoubles (mtrng s) 2.3 3.2 n) s
                    ]
    ]

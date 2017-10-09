module Main where

import Criterion.Main
import qualified Data.PRNG as RNG
import qualified Data.PRNG.MTRNG as MT
import qualified Data.PRNG.MWC as MWC
import System.Random.MWC
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import Graphics.Histogram






-- Our benchmark harness.
main = do
  let n = 1000000 :: Int
      μ = 1.0
      σ = 0.2
      lb = 0.0
      rb = 2.0
      s = 114
      mtrng = RNG.getRNG :: Int -> MT.MTRNG
      mwcrng = RNG.getRNG :: Int -> MWC.MWCRNG
  {-writeFile "test/binomial1.csv" $ -}
    {-(show $ RNG.binomialSample (mtrng s) n 0.5 n )-}
      normsample = RNG.normalSample (mtrng s) 1 4.3 n
      histnr = histogram binSturges normsample
      optsnr = Opts.title "normal" $ 
             Opts.yLabel "y" $ 
             Opts.xLabel "x" $ 
             defOpts histnr
      normtruncsample = RNG.truncatedNormalSample (mtrng s) 1 0.2 0 2.0 n 
      histtr = histogram binScott normtruncsample
      optstr = Opts.title "truncated" $ 
             Opts.yLabel "y" $ 
             Opts.xLabel "x" $ 
             defOpts histtr
  print $ show $ sum normtruncsample
  plotAdv "test/normal.eps" optsnr histnr
  plotAdv "test/trunc.eps" optstr histtr
  defaultMain [ 
                bgroup "mtrng" [ bench "normal sample" $ whnf (\s -> sum $ RNG.normalSample (mtrng s) μ σ n) s
                               , bench "truncated normal" $ whnf (\s -> sum (RNG.truncatedNormalSample (mtrng s) μ σ lb rb n)) s
                    ]
              , bgroup "MWC-sampling" [ bench "normal sample" $ whnf (\s -> sum $ RNG.normalSample (mwcrng s) μ σ n) s
                                     , bench "truncated normal" $ whnf (\s -> sum $ RNG.truncatedNormalSample (mwcrng s) μ σ lb rb n) s
                    ]
    ]

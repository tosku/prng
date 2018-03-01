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
import Data.Text.Lazy
import Data.Text.Lazy.IO as I (appendFile,writeFile)
import Data.Aeson.Text (encodeToLazyText)





-- Our benchmark harness.
main = do
  let n = 102400 :: Int
      μ = 1.0
      σ = 1.0
      lb = 0.0
      rb = 2.0
      s = 13358
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
      normtruncsample = RNG.truncatedNormalSample (mtrng s) 1 1.0 0 2.0 n 
      histtr = histogram binFreedmanDiaconis normtruncsample
      optstr = Opts.title "truncated sigma=1.0" $ 
             Opts.yLabel "#" $ 
             Opts.xLabel "x" $ 
             defOpts histtr
  print $ show $ sum normtruncsample
  let truncfile = "test/trunc1.0"
  plotAdv "test/normal.eps" optsnr histnr
  plotAdv (truncfile ++ ".eps") optstr histtr
  I.writeFile (truncfile ++ ".json") $ encodeToLazyText normtruncsample
  let nbench = 10
  defaultMain [ 
                bgroup "mtrng" [ bench "normal sample" $ whnf (\s -> sum $ RNG.normalSample (mtrng s) μ σ nbench) s
                               , bench "truncated normal" $ whnf (\s -> sum (RNG.truncatedNormalSample (mtrng s) μ σ lb rb nbench)) s
                    ]
              {-, bgroup "MWC-sampling" [ bench "normal sample" $ whnf (\s -> sum $ RNG.normalSample (mwcrng s) μ σ nbench) s-}
                                     {-, bench "truncated normal" $ whnf (\s -> sum $ RNG.truncatedNormalSample (mwcrng s) μ σ lb rb nbench) s-}
                    {-]-}
    ]

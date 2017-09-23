{-|
Module      : MTRNG
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

MT RNG Class definitions
 -}

module Data.MTRNG
    ( Seed
    , randomBools
    , randomDoubles
    , randomPermutation
    , sample
    ) where

import Data.Maybe
{-import qualified Data.List as List-}
import Data.List
import System.Random.Mersenne.Pure64 as MT


type Seed = Int
type RNG = Seed -> PureMT

getRNG :: RNG 
getRNG s = pureMT $ fromIntegral s

-- | returns a list of booleans given the length of the list list and the seed
randomBools :: Int -> Seed -> Double -> [Bool]
randomBools l s p        =
  let rndbls 0 _         = []
      rndbls n g         = b:(rndbls (n-1) g')
        where (d,g')     = MT.randomDouble g
              b          = d > p
  in rndbls l (getRNG s)


-- | permutates list using a seed
randomPermutation :: [a] -> Seed -> [a]
randomPermutation ls s = 
  let randoms = zip (randomDoubles (length ls) s) ls
   in map snd $ sortOn fst randoms

-- | Sample from list 
sample :: [a] -> Int -> Seed -> [a]
sample c n s 
  | n > length c = []
  | n < 0 = []
  | otherwise = take n $ randomPermutation c s

-- | returns a list of Doubles given the length of the list list and the Seed
randomDoubles :: Int -> Seed -> [Double]
randomDoubles l s = 
  let rnddbls 0 _ = []
      rnddbls n g = d:(rnddbls (n-1) g')
        where (d,g') = g `seq` MT.randomDouble g
  in rnddbls l (getRNG s)

-- ! Marsaglia algorithm (needs two seeds)
normalDistribution :: Int -> Seed -> Seed -> Double -> Double -> [Double]
normalDistribution n s1 s2 μ σ =
  -- let getNormal g = MT.randomDouble g)
  let normals 0 _ = []
      normals n g = d:(normals (n-1) g')
        where (d,g') = MT.randomDouble g
  in normals n (getRNG s1)

-- 
-- static bool hasSpare = false;
-- 
-- static double spare;
-- 
-- 
-- if(hasSpare) {
-- 
--   hasSpare = false;
-- 
--   return mean + stdDev * spare;
-- 
-- }
-- 
-- 
-- hasSpare = true;
-- 
-- static double u, v, s;
-- 
-- do {
-- 
--   u = (rand() / ((double) RAND_MAX)) * 2.0 - 1.0;
-- 
--   v = (rand() / ((double) RAND_MAX)) * 2.0 - 1.0;
-- 
--   s = u * u + v * v;
-- 
-- }
-- 
-- while( (s >= 1.0) || (s == 0.0) );
-- 
-- 
-- s = sqrt(-2.0 * log(s) / s);
-- 
-- spare = v * s;
-- 
-- return mean + stdDev * u * s;e
--
randomDouble = MT.randomDouble

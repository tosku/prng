{-|
Module      : PRNG
Description : Class definitions of PRNG api

Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

PRNG Class definitions
 -}

module Data.PRNG
    ( Seed
    , RNG (..)
    , randomBools
    , randomPermutation
    , sample
    ) where

import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.IntMap as IM


type Seed = Int
class RNG r where 
  getRNG :: Seed -> r
  randomDouble :: r -> (Double, r)
  randomDoubles :: r -> Int -> [Double]


-- | booleans given the length of the list the probability p and the seed
randomBools :: RNG r => r -> Int -> Double -> [Bool]
randomBools r l p        =
  let rndbls 0 _         = []
      rndbls n g         = b:(rndbls (n-1) g')
        where (d,g')     = randomDouble g
              b          = d > p
  in rndbls l r

-- | Permutates list using a RNG, given that the list has fewer elements than
-- the minimum between  2^64 which is the precision of Double and the period of the prng,the position probability of every element is uniform.
randomPermutation :: RNG r => r -> [a] -> [a]
randomPermutation r ls = 
  let randoms = zip (randomDoubles r (length ls)) ls
   in map snd $ sortOn fst randoms

-- | Sample from list 
sample :: RNG r => r -> Int -> [a] -> [a]
sample r k c
  | k < 0  || k > length c = []
  | otherwise = take k $ randomPermutation r c
     

-- ! Marsaglia algorithm (needs two seeds)
{-normalDistribution :: Seed -> Seed -> Int -> Double -> Double -> [Double]-}
{-normalDistribution n s1 s2 μ σ =-}
  {--- let getNormal g = MWC.randomDouble g)-}
  {-let normals 0 _ = []-}
      {-normals n g = d:(normals (n-1) g')-}
        {-where (d,g') = randomDouble g-}
  {-in normals n (getRNG s1)-}

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

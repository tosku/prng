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
    , normalSample
    , truncatedNormalSample
    ) where

import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Number.Erf
import qualified Numeric.SpecFunctions as NS


type Seed = Int
class RNG r where 
  getRNG :: Seed -> r
  randomDouble :: r -> (Double, r)
  uniformSample :: r -> Int -> [Double]


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
  let randoms = zip (uniformSample r (length ls)) ls
   in map snd $ sortOn fst randoms

-- | Sample from list 
sample :: RNG r => r -> Int -> [a] -> [a]
sample r k c
  | k < 0  || k > length c = []
  | otherwise = take k $ randomPermutation r c
     
-- | List of doubles from nornal distribution given 
-- RNG -> μ -> σ -> length of list
normalSample :: RNG r => r -> Double -> Double -> Int -> [Double]
normalSample r μ σ n =
  let uds = uniformSample r n
      invx x = σ * x + μ
   in map (invx . invnormcdf) uds

-- | Sample truncated nomral μ -> σ -> left bound -> right bound -> length of list
truncatedNormalSample :: RNG r => r -> Double -> Double -> Double -> Double -> Int -> [Double]
truncatedNormalSample r μ σ f t n =
  let uds = uniformSample r n
      sqt = sqrt 2
      invtruncnormcdf y = μ - sqt * σ * inverf (
        - ((-1) + y) * erf((-f + μ)/(sqt * σ)) +
        y * erf((μ - t)/(sqt * σ))
        )
   in map invtruncnormcdf uds


-- | probability of i successes of binomial distribution

-- | Inverted binomial cumulative distribution function
{-invbinomcdf :: Double -> Int -> Double -> Int-}

-- | List of doubles from the binomial distribution given 
-- RNG -> n -> p -> length of list
binomialSample :: RNG r => r -> Int -> Double -> Int -> [Int]
binomialSample r n p l =
  let uds = uniformSample r l
      invbinomcdf a n p = let sumpr i = foldl (\ac i -> ac + (binomialProb i n p)) 0 [1..i] 
                              binomialProb i n p = n `NS.choose` i * p^^i * (1-p)^^(n-i)
                              firstBigger x a 
                                | sumpr (x-1) > a = x
                                | x > n = n
                                | otherwise = firstBigger (x+1) a
                              in firstBigger 1 a
 in map (\a -> invbinomcdf a n p) uds

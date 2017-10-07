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
    , normalDoubles
    , randomBools
    , randomPermutation
    , sample
    ) where

import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Number.Erf


type Seed = Int
class RNG r where 
  getRNG :: Seed -> r
  randomDouble :: r -> (Double, r)
  uniformDoubles :: r -> Int -> [Double]


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
  let randoms = zip (uniformDoubles r (length ls)) ls
   in map snd $ sortOn fst randoms

-- | Sample from list 
sample :: RNG r => r -> Int -> [a] -> [a]
sample r k c
  | k < 0  || k > length c = []
  | otherwise = take k $ randomPermutation r c
     
-- | List of doubles from nornal distribution given 
-- RNG -> μ -> σ -> length of list
normalDoubles :: RNG r => r -> Double -> Double -> Int -> [Double]
normalDoubles r μ σ n =
  let uds = uniformDoubles r n
      invx x = σ * x + μ
   in map (invx . invnormcdf) uds

{-|
Module      : PRNG.MTRNG
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

MT RNG Class definitions
 -}

module Data.PRNG.MTRNG
    ( MTRNG (..)
    ) where

import qualified Data.PRNG as PRNG
import qualified System.Random.Mersenne.Pure64 as MT

type MTRNG = MT.PureMT

instance PRNG.RNG MT.PureMT where
  getRNG s = MT.pureMT $ fromIntegral s
  randomDouble = MT.randomDouble
  randomDoubles r l = 
    let rnddbls 0 _ = []
        rnddbls n g = d:(rnddbls (n-1) g')
          where (d,g') = PRNG.randomDouble g
    in rnddbls l r

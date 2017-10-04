{-|
Module      : PRNG.ACMWC
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

MWC RNG Class definitions
 -}

module Data.PRNG.ACMWC
    ( Seed
    , MWCRNG (..)
    ) where

import qualified Data.PRNG as PRNG
import qualified Random.MWC.Pure as MWC


type Seed = PRNG.Seed
type MWCRNG = MWC.Seed

instance PRNG.RNG MWC.Seed where
  getRNG s = MWC.seed $ [fromIntegral s]
  randomDouble = MWC.unit_random
  randomDoubles r l = 
    let rnddbls 0 _ = []
        rnddbls n g = d:(rnddbls (n-1) g')
          where (d,g') = PRNG.randomDouble g
    in rnddbls l r


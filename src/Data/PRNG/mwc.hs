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

module Data.PRNG.MWC
    ( MWCRNG  (..)
    ) where

import Data.List
import qualified Data.Vector as V
import Control.Monad
{-import Control.Monad.ST-}
{-import Control.Monad.ST.Lazy-}
import System.Random.MWC
import qualified Data.PRNG as PRNG
import Control.Monad.Primitive
import Control.Monad.ST

type MWCRNG = Seed

instance PRNG.RNG Seed where
  getRNG s = toSeed (V.singleton (fromIntegral s))
  randomDouble s = runST $ do 
    x <- restore s
    y <- uniform x
    s' <- save x
    return (y, s')
  uniformDoubles s n = runST $ do
    gen <- restore s
    vs <- uniformVector gen n
    return $ V.toList vs


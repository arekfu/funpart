{-# LANGUAGE FlexibleContexts #-}

module MC
( MC
, Seed
, uniform
, uniforms
, sampleExp
, sampleV
, sampleUniformV
, getGen
, sampleIsoVec
) where

import System.Random
import Control.Monad.State
import qualified Data.Vector as V

import Core
import Vec

type Seed = Int

type MC = State StdGen

getGen :: MonadState StdGen m => m StdGen
getGen = get

uniform :: (Random a, Fractional a, MonadState StdGen m) => m a
uniform = do
    gen <- getGen
    let (xi, gen') = randomR (0.0, 1.0) gen
    put gen'
    return xi

uniforms :: (Random a, Fractional a, MonadState StdGen m)
         => Int
         -> m [a]
uniforms n = forM [1..n] $ const uniform

-- | Sample from an exponential distribution of the form
-- @
-- f(x) = exp(-&#x3BB; x)/&#x3BB;
-- @
sampleExp :: (Random a, Floating a, MonadState StdGen m)
          => a  -- ^ The distribution
          -> m a
sampleExp lambda = do xi <- uniform
                      return $ (-lambda) * log xi

sampleV :: (Num a, Ord a, Fractional a, Random a)
        => V.Vector a
        -> a
        -> Maybe Int
sampleV v xi = do
    let v' = V.scanl1' (+) v
    i <- V.findIndex (>xi) v'
    return $ i+1

sampleUniformV :: (Num a, Ord a, Fractional a, Random a, MonadState StdGen m)
        => V.Vector a
        -> m (Maybe Int)
sampleUniformV v = do
    xi <- uniform
    return $ sampleV v xi

sampleIsoVec :: MonadState StdGen m => FPFloat -> m FPVec3
sampleIsoVec norm = do
    u <- uniform
    v <- uniform
    let theta = 2.0 * u - 1.0
        phi = twoPi * v
        x = norm * sin theta * cos phi
        y = norm * sin theta * sin phi
        z = norm * cos theta
     in return $ cart x y z

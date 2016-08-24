module MC
( MC
, Seed
, runMC
, evalMC
, execMC
, uniform
, uniforms
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

getGen :: MC StdGen
getGen = get

runMC :: State s a -> s -> (a, s)
runMC = runState

evalMC :: State s a -> s -> a
evalMC = evalState

execMC :: State s a -> s -> s
execMC = execState

uniform :: (Random a, Fractional a) => MC a
uniform = do
    gen <- getGen
    let (xi, gen') = randomR (0.0, 1.0) gen
    put gen'
    return xi

uniforms :: (Random a, Fractional a)
         => Int
         -> MC [a]
uniforms n = forM [1..n] $ const uniform

sampleV :: (Num a, Ord a, Fractional a, Random a)
        => V.Vector a
        -> a
        -> Maybe Int
sampleV v xi = do
    let v' = V.scanl1' (+) v
    i <- V.findIndex (>xi) v'
    return $ i+1

sampleUniformV :: (Num a, Ord a, Fractional a, Random a)
        => V.Vector a
        -> MC (Maybe Int)
sampleUniformV v = do
    xi <- uniform
    return $ sampleV v xi

sampleIsoVec :: FPFloat -> MC FPVec3
sampleIsoVec norm = do
    u <- uniform
    v <- uniform
    let theta = 2.0 * u - 1.0
        phi = twoPi * v
        x = norm * sin theta * cos phi
        y = norm * sin theta * sin phi
        z = norm * cos theta
     in return $ cart x y z

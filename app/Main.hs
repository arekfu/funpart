module Main where

import qualified Data.Sequence as S
import Data.List (foldl')

import SimSetup
import Problem
import CrossSection
import Particle
import Source
import Source.Distributions
import Track
import Vec

setup :: SimSetup
setup = SimSetup { theXSec = xSec
                 , initialSeed = seed
                 , nShots = shots
                 , source = aSource
                 , scores = []
                 }
        where xSec = CrossSection $ ConstantXS totXSec absXSec
              totXSec = 1.0
              absXSec = 0.9
              seed = 123456
              shots = 1000
              aSource = Source $ FactorizedSource Neutron (PointwiseSpaceDistribution (Pos zero)) (IsoMonoDistribution 1)

average :: (Num a, Fractional a) => [a] -> a
average l = tot / fromIntegral n
    where (tot, n) = foldl' acc (0, 0 :: Int) l
          acc (x, m) y = (x+y, m+1)

main :: IO ()
--main = forM_ tracks $ \track -> print $ S.length $ _trackPoints track
--    where tracks = snd $ runProblem fixedSourceProblem setup
main = print ((average $ map (fromIntegral . S.length . _trackPoints) tracks) :: Double)
    where tracks = snd $ runProblem fixedSourceProblem setup

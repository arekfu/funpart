module Main where

import Data.List (foldl')

import Physics.FunPart.SimSetup
import Physics.FunPart.Problem
import Physics.FunPart.CrossSection
import Physics.FunPart.Particle
import Physics.FunPart.Score
import Physics.FunPart.Source
import Physics.FunPart.Source.Distributions
import Physics.FunPart.Stat
import Physics.FunPart.Vec

setup :: SimSetup
setup = SimSetup { theXSec = xSec
                 , initialSeed = seed
                 , nShots = shots
                 , source = aSource
                 , scores = [TrackLength empty]
                 }
        where xSec = CrossSection $ ConstantXS totXSec absXSec
              totXSec = 1.0
              absXSec = 0.1
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

--main = do print ((average $ map (fromIntegral . S.length . _trackPoints) tracks) :: Double)
--          print expectedNPoints
--    where tracks          = snd $ runProblem fixedSourceProblem setup
--          dummyDP         = mkDynParticle (Pos zero) (Mom $ cart 0 0 1)
--          theXS           = theXSec setup
--          absXS           = getAbsXS theXS dummyDP
--          totXS           = getTotXS theXS dummyDP
--          pAbs            = absXS/totXS
--          expectedNPoints = 1 + 1/pAbs

main = do putStrLn $ display calculatedNpts
          print expectedNPts
          print $ calculatedNpts `sigmasFrom` expectedNPts
    where score        = head $ runSimulation fixedSourceProblem setup
          (TrackLength calculatedNpts) = score
          dummyDP      = mkDynParticle (Pos zero) (Mom kHat)
          theXS        = theXSec setup
          absXS        = getAbsXS theXS dummyDP
          totXS        = getTotXS theXS dummyDP
          pAbs         = absXS/totXS
          expectedNPts = 1 + 1/pAbs

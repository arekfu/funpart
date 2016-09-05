module Main where

import Control.Monad

import SimSetup
import Problem
import CrossSection
import Particle
import Source
import Source.Distributions
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
              absXSec = 0.1
              seed = 123456
              shots = 3
              aSource = Source $ FactorizedSource Neutron (PointwiseSpaceDistribution (Pos zero)) (IsoMonoDistribution 1)

main :: IO ()
main = forM_ tracks $ \track -> print track
    where tracks = snd $ runProblem fixedSourceProblem setup

module Main where

import SimSetup
import Score
import Particle
import Source
import Source.Distributions
import CrossSection
import Vec
import Problem

setup :: SimSetup
setup = SimSetup { theXSec = CrossSection $ ConstantXS 1.0 0.1
                 , initialSeed = 123
                 , nShots = 1
                 , source = Source $ FactorizedSource Neutron (PointwiseSpaceDistribution (Pos $ cart 0.0 0.0 0.0)) (IsoMonoDistribution 1.0)
                 , scores = [Score $ CollFlux 0.0]
                 }

main :: IO ()
main = print $ map display $ runProblem fixedSourceProblem setup

module SimSetupTest
( ASimSetup(..)
) where

import Test.QuickCheck

import Core
import VecTest (AVec3, aVec)
import SimSetup
import CrossSection
import Particle
import Source
import Source.Distributions

newtype ASimSetup = ASimSetup SimSetup deriving Show

instance Arbitrary ASimSetup where
    arbitrary = do totXSec <- choose (0.01, 10.0) :: Gen FPFloat
                   absXSec <- choose (0.001, totXSec) :: Gen FPFloat
                   seed <- arbitrary :: Gen (Positive (Large Int))
                   shots <- choose (1, 5) :: Gen Int
                   particleType <- elements [Photon, Neutron]
                   sourcePosition <- arbitrary :: Gen (AVec3 FPFloat)
                   sourceMomentum <- arbitrary
                   return $ ASimSetup SimSetup { theXSec = CrossSection $ ConstantXS totXSec absXSec
                                   , initialSeed = getLarge $ getPositive seed
                                   , nShots = shots
                                   , source = Source $ FactorizedSource particleType (PointwiseSpaceDistribution (Pos $ aVec sourcePosition)) (IsoMonoDistribution sourceMomentum)
                                   , scores = []
                                   }

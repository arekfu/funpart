{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module ParticleTest
( runTests
, AParticleType(..)
, ADynParticle(..)
, AParticle(..)
) where

import Test.QuickCheck
import Data.Maybe (isNothing)

import qualified VecTest as VT
import VecSpace
import Particle
import Approx

newtype AParticleType = AParticleType { aParticleType :: ParticleType }
                        deriving (Show, Eq)

newtype ADynParticle = ADynParticle { aDynParticle :: DynParticle }
                       deriving (Show, Eq)

newtype AParticle = AParticle { aParticle :: Particle }
                    deriving (Show, Eq)

instance Arbitrary AParticleType where
    arbitrary = elements $ map AParticleType [Neutron, Photon]

instance Arbitrary ADynParticle where
    arbitrary = do ar <- arbitrary
                   ap <- arbitrary
                   return $ ADynParticle $ mkDynParticle (Pos $ VT.aVec ar) (Mom $ VT.aVec ap)

instance Arbitrary AParticle where
    arbitrary = do atype <- arbitrary
                   adyn  <- arbitrary
                   return $ AParticle $ mkParticle (aParticleType atype) (aDynParticle adyn)

instance Approx ADynParticle where
    distance (ADynParticle dp0) (ADynParticle dp1) = distance (pos dp0) (pos dp1) + distance (mom dp0) (mom dp1)

instance Approx AParticle where
    distance (AParticle p0) (AParticle p1) = distance (ADynParticle $ dynPart p0) (ADynParticle $ dynPart p1) + (if ptype p0 == ptype p1 then 0.0 else 1.0)

prop_pushNotFails :: Distance -> ADynParticle -> Property
prop_pushNotFails dist (ADynParticle dpart) = mag (getMom $ mom dpart) > 0.0 ==>
                                          case push dist dpart of
                                            Nothing -> False
                                            _       -> True

prop_pushFailsOnZeroMom :: Distance -> ADynParticle -> Bool
prop_pushFailsOnZeroMom dist (ADynParticle dpart) =
    let dpart' = dpart { mom = Mom zero }
     in isNothing $ push dist dpart'

return []
runTests :: IO Bool
runTests = $quickCheckAll

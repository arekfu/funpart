{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module ParticleTest
( runTests
, AParticleType(..)
, ADynParticle(..)
, AParticle(..)
) where

import Test.QuickCheck
import Data.Maybe (isNothing, isJust, fromMaybe)

import qualified VecTest as VT
import VecSpace
import Particle
import Approx
import Control.Lens (view)

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
    distance (ADynParticle dp0) (ADynParticle dp1) =
      let dpos = distance (view position dp0) (view position dp1)
          dmom = distance (view momentum dp0) (view momentum dp1)
       in dpos+dmom

instance Approx AParticle where
    distance (AParticle p0) (AParticle p1) =
      let dpart = distance (ADynParticle $ view dynPart p0) (ADynParticle $ view dynPart p1)
          dtype = if view ptype p0 == view ptype p1 then 0.0 else 1.0
       in dpart+dtype

prop_pushNotFails :: Distance -> ADynParticle -> Property
prop_pushNotFails dist (ADynParticle dpart) =
    mag (view (momentum.momentumVec) dpart) > 0.0 ==> isJust $ push dist dpart

prop_pushFailsOnZeroMom :: Distance -> ADynParticle -> Bool
prop_pushFailsOnZeroMom dist (ADynParticle dpart) =
    let dpart' = dpart { _momentum = Mom zero }
     in isNothing $ push dist dpart'

backAndForth :: Distance -> DynParticle -> DynParticle
backAndForth dist dp =
    fromMaybe dp $ do
        dp'  <- push dist dp
        push (-dist) dp'

prop_pushAndComeBack :: Distance -> ADynParticle -> Property
prop_pushAndComeBack dist (ADynParticle dpart) =
    mag (view (momentum.momentumVec) dpart) > 0.0 ==>
    let dpart' = backAndForth dist dpart
     in ADynParticle dpart ~== ADynParticle dpart'

return []
runTests :: IO Bool
runTests = $quickCheckAll

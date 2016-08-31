{-# LANGUAGE TemplateHaskell #-}

module ProblemTest
( runTests
) where

import Test.QuickCheck
import Control.Lens
import Data.Maybe (fromJust)

import ParticleTest hiding (runTests)
import SimSetupTest

import Approx
import Step
import Particle
import Problem
import Problem.Common
import Vec

prop_nextStepType :: AParticle -> ASimSetup -> Property
prop_nextStepType (AParticle p) (ASimSetup setup) =
        counterexample (show stepPoint) $
        mag (p^.pMomentumVec) > 0.0 ==>
        case stepPoint^.stepPointType of
            SourceStepPoint -> False
            _               -> True
        where stepPoint = fst $ runAnyProblem (nextStep p) setup

prop_nextStepAligned :: AParticle -> ASimSetup -> Property
prop_nextStepAligned (AParticle p) (ASimSetup setup) =
        mag (p^.pMomentumVec) > 0.0 ==>
        initialMomentumUnitVec ~== displacementUnitVec
        where stepPoint = fst $ runAnyProblem (nextStep p) setup
              initialMomentumUnitVec = fromJust $ toUnitVector $ p^.pMomentumVec
              (Pos finalPositionVec) = stepPoint^.stepPointVertex
              initialPositionVec     = p^.pPositionVec
              displacement           = finalPositionVec -: initialPositionVec
              displacementUnitVec    = fromJust $ toUnitVector displacement

prop_solveConverges :: AParticle -> ASimSetup -> Bool
prop_solveConverges (AParticle p) (ASimSetup setup) = runAnyProblem (solve p) setup `seq` True

return []
runTests :: IO Bool
runTests = $quickCheckAll

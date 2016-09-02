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
import Track
import Vec

prop_nextStepType :: AParticle -> ASimSetup -> Property
prop_nextStepType (AParticle p) (ASimSetup setup) =
        counterexample (show stepPoint) $
        mag (p^.pMomentumVec) > 0.0 ==>
        case stepPoint^.stepPointType of
            SourceStepPoint -> False
            _               -> True
        where stepPoint = fst $ runProblem (nextStep p) setup

prop_nextStepAligned :: AParticle -> ASimSetup -> Property
prop_nextStepAligned (AParticle p) (ASimSetup setup) =
        mag (p^.pMomentumVec) > 0.0 ==>
        initialMomentumUnitVec ~== displacementUnitVec
        where stepPoint = fst $ runProblem (nextStep p) setup
              initialMomentumUnitVec = fromJust $ toUnitVector $ p^.pMomentumVec
              (Pos finalPositionVec) = stepPoint^.stepPointVertex
              initialPositionVec     = p^.pPositionVec
              displacement           = finalPositionVec -: initialPositionVec
              displacementUnitVec    = fromJust $ toUnitVector displacement

prop_firstStepPointIsSource :: AParticle -> ASimSetup -> Property
prop_firstStepPointIsSource (AParticle p) (ASimSetup setup) =
        counterexample (show firstStep) $
        mag (p^.pMomentumVec) > 0.0 ==>
        firstStep^.stepPointType == SourceStepPoint
        where firstStep = last $ fst $ runProblem (steps p) setup

firstTrackPointIsSource :: Track -> Bool
firstTrackPointIsSource track = last (track^.trackPoints) ^. pointType == SourcePoint

prop_solveFromSource :: AParticle -> ASimSetup -> Property
prop_solveFromSource (AParticle p) (ASimSetup setup) =
        counterexample (show track) $
        mag (p^.pMomentumVec) > 0.0 ==>
        firstTrackPointIsSource track
        where track = fst $ fst $ runProblem (solve p) setup

return []
runTests :: IO Bool
runTests = $quickCheckAll

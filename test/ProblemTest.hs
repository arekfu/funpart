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
import Particle
import Problem
import Problem.Common
import Track
import Vec

prop_nextStepType :: AParticle -> ASimSetup -> Property
prop_nextStepType (AParticle p) (ASimSetup setup) =
        counterexample (show stepPoint) $
        mag (p^.pMomentumVec) > 0.0 ==>
        case stepPoint^.pointType of
            SourcePoint -> False
            _           -> True
        where stepPoint = fst $ runProblem (nextStep p) setup

prop_nextStepAligned :: AParticle -> ASimSetup -> Property
prop_nextStepAligned (AParticle p) (ASimSetup setup) =
        mag (p^.pMomentumVec) > 0.0 ==>
        initialMomentumUnitVec ~== displacementUnitVec
        where trackPoint = fst $ runProblem (nextStep p) setup
              initialMomentumUnitVec = fromJust $ toUnitVector $ p^.pMomentumVec
              (Pos finalPositionVec) = trackPoint^.pointVertex
              initialPositionVec     = p^.pPositionVec
              displacement           = finalPositionVec -: initialPositionVec
              displacementUnitVec    = fromJust $ toUnitVector displacement

firstStepPointIs :: (Particle -> Problem [TrackPoint])
                 -> TrackPointType
                 -> AParticle
                 -> ASimSetup
                 -> Property
firstStepPointIs stepper aPointType (AParticle p) (ASimSetup setup) =
        counterexample (show firstStep) $
        mag (p^.pMomentumVec) > 0.0 ==>
        firstStep^.pointType == aPointType
        where firstStep = last $ fst $ runProblem (stepper p) setup

prop_firstStepPointIsSource :: AParticle -> ASimSetup -> Property
prop_firstStepPointIsSource = firstStepPointIs stepsFromSource SourcePoint

prop_firstStepPointIsSecondary :: AParticle -> ASimSetup -> Property
prop_firstStepPointIsSecondary = firstStepPointIs stepsFromSecondary SecondaryPoint

firstTrackPointIsSource :: Track -> Bool
firstTrackPointIsSource track = last (track^.trackPoints) ^. pointType == SourcePoint

prop_solveFromSource :: AParticle -> ASimSetup -> Property
prop_solveFromSource (AParticle p) (ASimSetup setup) =
        counterexample (show track) $
        mag (p^.pMomentumVec) > 0.0 ==>
        firstTrackPointIsSource track
        where track = fst $ runProblem (solve p) setup

prop_solveAllFromSource :: AParticle -> ASimSetup -> Property
prop_solveAllFromSource (AParticle p) (ASimSetup setup) =
        counterexample (show tracks) $
        mag (p^.pMomentumVec) > 0.0 ==>
        all firstTrackPointIsSource tracks
        where tracks = snd $ runProblem (solveAll [p]) setup

return []
runTests :: IO Bool
runTests = $quickCheckAll

{-# LANGUAGE TemplateHaskell #-}

module Physics.FunPart.ProblemTest
( runTests
) where

import Test.QuickCheck
import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Sequence (Seq, viewl, ViewL(..))

import Physics.FunPart.ParticleTest hiding (runTests)
import Physics.FunPart.SimSetupTest

import Physics.FunPart.Approx
import Physics.FunPart.Particle
import Physics.FunPart.Problem
import Physics.FunPart.Problem.Common
import Physics.FunPart.Track
import Physics.FunPart.Vec

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

firstStepPointIs :: (Particle -> Problem (Seq TrackPoint))  -- ^ The function that generates the sequence of TrackPoints.
                 -> TrackPointType                          -- ^ The expected TrackPointType for the first point.
                 -> AParticle                               -- ^ The particle to simulate.
                 -> ASimSetup                               -- ^ The simulation setup
                 -> Property
firstStepPointIs stepper expected (AParticle p) (ASimSetup setup) =
        counterexample (show firstPoint) $
        mag (p^.pMomentumVec) > 0.0 ==>
        case firstPoint of
            point :< _ -> point^.pointType == expected
            EmptyL     -> False
        where firstPoint = viewl $ fst $ runProblem (stepper p) setup

prop_firstStepPointIsSource :: AParticle -> ASimSetup -> Property
prop_firstStepPointIsSource = firstStepPointIs stepsFromSource SourcePoint

prop_firstStepPointIsSecondary :: AParticle -> ASimSetup -> Property
prop_firstStepPointIsSecondary = firstStepPointIs stepsFromSecondary SecondaryPoint

firstTrackPointIsSource :: Track -> Bool
firstTrackPointIsSource track = case firstPoint of
    point :< _ -> point^.pointType == SourcePoint
    EmptyL     -> False
    where firstPoint = viewl $ track^.trackPoints

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

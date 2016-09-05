{-# LANGUAGE FlexibleContexts #-}

module Problem.Common
( runHistory
, nextStep
, steps
, stepsFromSource
, stepsFromSecondary
, solve
, solveAll
) where

import Control.Monad.RWS.Strict
import System.Random (StdGen)
import Control.Lens
import Data.Maybe (fromJust)

import qualified SimSetup
import CrossSection (getAbsXS, getTotXS, CrossSectionValue)
import Particle
import Track
import Source
import MC (uniform, sampleExp, sampleIsoVec)
import VecSpace (mag)

distanceToCollision :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m)
                    => Particle
                    -> m Distance
distanceToCollision p = do xSecs <- asks SimSetup.theXSec
                           let totXSec = getTotXS xSecs p
                           sampleExp (1.0/totXSec)

sampleIsoScattering :: MonadState StdGen m
                    => Particle
                    -> m Particle
sampleIsoScattering p = do mom' <- sampleIsoVec $ mag $ p^.pMomentumVec
                           return $ set pMomentum (Mom mom') p

doCollision :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
            => Particle
            -> m TrackPoint
doCollision p = do xSecs <- asks SimSetup.theXSec
                   let totXSec = getTotXS xSecs p
                       absXSec = getAbsXS xSecs p
                       ratio = absXSec / totXSec
                   xi <- uniform
                   if xi <= ratio
                       then return $ mkAbsorption p
                       else doScattering totXSec p

mkSource :: Particle -> TrackPoint
mkSource p = TrackPoint { _pointType = SourcePoint
                        , _pointVertex = p^.pPosition
                        , _pointMomentum = p^.pMomentum
                        , _pointWeight = p^.pWeight
                        }

mkSecondary :: Particle -> TrackPoint
mkSecondary p = TrackPoint { _pointType = SecondaryPoint
                           , _pointVertex = p^.pPosition
                           , _pointMomentum = p^.pMomentum
                           , _pointWeight = p^.pWeight
                           }

mkAbsorption :: Particle -> TrackPoint
mkAbsorption p = TrackPoint { _pointType = EndPoint
                            , _pointVertex = p^.pPosition
                            , _pointMomentum = p^.pMomentum
                            , _pointWeight = p^.pWeight
                            }

doScattering :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
             => CrossSectionValue -> Particle -> m TrackPoint
doScattering xsec p = do p' <- sampleIsoScattering p
                         secs <- stepsFromSecondary p'
                         let point = CollisionPoint { _collisionXSec = xsec
                                                    , _secondaries = [Track secs] }
                         return TrackPoint { _pointType = point
                                           , _pointVertex = p^.pPosition
                                           , _pointMomentum = p^.pMomentum
                                           , _pointWeight = p^.pWeight
                                           }

-- | Take one transport step.
nextStep :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
         => Particle
         -> m TrackPoint
nextStep p = do dist <- distanceToCollision p
                let p'   = fromJust $ push dist p   -- ugh, fromJust! FIXME
                doCollision p'

steps :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
      => [TrackPoint]
      -> Particle
      -> m [TrackPoint]
steps stepsSoFar p = do next <- nextStep p
                        let typ      = next^.pointType
                            newSteps = next : stepsSoFar
                        case typ of
                            EndPoint           -> return newSteps
                            CollisionPoint _ _ -> return newSteps
                            SourcePoint        -> error "SourceStepPoint generated along a track"
                            SecondaryPoint     -> error "SecondaryStepPoint generated along a track"

stepsFromSource :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
                => Particle
                -> m [TrackPoint]
stepsFromSource = stepsFrom mkSource


stepsFromSecondary :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
                   => Particle
                   -> m [TrackPoint]
stepsFromSecondary = stepsFrom mkSecondary

stepsFrom :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
          => (Particle -> TrackPoint)
          -> Particle
          -> m [TrackPoint]
stepsFrom maker p = steps [firstStep] p
    where firstStep = maker p


-- | Solve one transport history.
solve :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)  -- ugh
         => Particle    -- ^ The particle to transport
         -> m Track     -- ^ The list of tracks and the list of secondaries
solve p = Track <$> stepsFromSource p

-- | Completely solve one transport history.
solveAll :: (MonadReader SimSetup.SimSetup m, MonadState StdGen m, MonadWriter [Track] m)
         => [Particle]  -- ^ The particles to transport
         -> m ()        -- ^ The tracks generated by the particles
solveAll [] = return ()
solveAll (p:ps) = do track <- solve p
                     tell [track]
                     solveAll ps

runHistory :: (MonadReader SimSetup.SimSetup m, MonadState StdGen m, MonadWriter [Track] m) => m ()
runHistory = do
    (SimSetup.SimSetup _ _ _ source _) <- ask
    particles <- sampleParticles source
    solveAll particles

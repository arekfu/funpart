{-# LANGUAGE FlexibleContexts #-}

module Physics.FunPart.Problem.Common
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
import Data.Sequence (Seq, singleton)

import qualified Physics.FunPart.SimSetup as SimSetup
import Physics.FunPart.CrossSection (getAbsXS, getTotXS, CrossSectionValue)
import Physics.FunPart.Particle
import Physics.FunPart.Track
import Physics.FunPart.Source
import Physics.FunPart.MC (uniform, sampleExp, sampleIsoVec)
import Physics.FunPart.VecSpace (mag)

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
                       else doElasticScattering totXSec p

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
mkAbsorption p = TrackPoint { _pointType = Absorption
                            , _pointVertex = p^.pPosition
                            , _pointMomentum = p^.pMomentum
                            , _pointWeight = p^.pWeight
                            }

doElasticScattering :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
                      => CrossSectionValue -> Particle -> m TrackPoint
doElasticScattering xsec p =
    do p' <- sampleIsoScattering p
       let point = CollisionPoint { _collisionXSec = xsec
                                  , _collision = Elastic p'
                                  }
       return TrackPoint { _pointType = point
                         , _pointVertex = p^.pPosition
                         , _pointMomentum = p^.pMomentum
                         , _pointWeight = p^.pWeight
                         }

{-
doInelasticScattering :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m) 
                      => CrossSectionValue -> Particle -> m TrackPoint
doInelasticScattering xsec p =
    do p' <- sampleIsoScattering p
       rest <- stepsFromSecondary p'
       let point = CollisionPoint { _collisionXSec = xsec
                                  , _collision = Inelastic [Track rest]
                                  }
       return TrackPoint { _pointType = point
                         , _pointVertex = p^.pPosition
                         , _pointMomentum = p^.pMomentum    -- FIXME: reduce the particle energy
                         , _pointWeight = p^.pWeight
                         }
-}

-- | Take one transport step.
nextStep :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
         => Particle
         -> m TrackPoint
nextStep p = do dist <- distanceToCollision p
                let p'   = fromJust $ push dist p   -- ugh, fromJust! FIXME
                doCollision p'

steps :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
      => Seq TrackPoint
      -> Particle
      -> m (Seq TrackPoint)
steps stepsSoFar p = do next <- nextStep p
                        let typ      = next^.pointType
                            newSteps = stepsSoFar |> next
                        case typ of
                            Absorption            -> return newSteps
                            CollisionPoint _ coll -> case coll of
                                Inelastic _       -> return newSteps
                                Elastic p'        -> steps newSteps p'
                            SourcePoint           -> error "SourceStepPoint generated along a track"
                            SecondaryPoint        -> error "SecondaryStepPoint generated along a track"

stepsFromSource :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
                => Particle
                -> m (Seq TrackPoint)
stepsFromSource = stepsFrom mkSource


stepsFromSecondary :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
                   => Particle
                   -> m (Seq TrackPoint)
stepsFromSecondary = stepsFrom mkSecondary

stepsFrom :: (MonadState StdGen m, MonadReader SimSetup.SimSetup m, MonadWriter [Track] m)
          => (Particle -> TrackPoint)
          -> Particle
          -> m (Seq TrackPoint)
stepsFrom maker p = steps (singleton firstStep) p
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

{-# LANGUAGE GADTs, FlexibleContexts #-}

module Physics.FunPart.Source
( SourceLike(..)
, Source(..)
, FactorizedSource(..)
, SourceIntensity
) where

import System.Random (StdGen)
import Control.Monad.State.Class (MonadState)

import Physics.FunPart.Core
import Physics.FunPart.Particle
import Physics.FunPart.Source.Distributions

type SourceIntensity = FPFloat

class SourceLike a where
    sampleParticles :: MonadState StdGen m => a -> m [Particle]

-- wrapper GADT for heterogeneous collections
data Source where
    Source :: SourceLike a => a -> Source

instance SourceLike Source where
    sampleParticles (Source a) = sampleParticles a

data FactorizedSource where
    FactorizedSource :: (SpaceDistribution a, MomentumDistribution b) =>
        ParticleType -> a -> b -> FactorizedSource

instance SourceLike FactorizedSource where
    sampleParticles (FactorizedSource typ spaceD momD) = do
        r <- samplePosition spaceD
        p <- sampleMomentum momD
        return [mkParticle typ r p]


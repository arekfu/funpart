{-# LANGUAGE GADTs #-}

module Source
( SourceLike(..)
, Source(..)
, FactorizedSource(..)
, SourceIntensity
) where

import Core
import Particle
import Source.Distributions
import MC

type SourceIntensity = FPFloat

class SourceLike a where
    sampleParticles :: a -> MC [Particle]

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


{-# LANGUAGE TemplateHaskell #-}

module Source
( Source(..)
, FactorizedSource(..)
, SpaceDistribution(..)
, center
, MomentumDistribution(..)
, momentumValue
, PointwiseSpaceDistribution(..)
, IsoMonoDistribution(..)
, SourceIntensity
, PointwiseIsoMonoSource
, mkPWIMSource
) where

import Control.Lens

import Core
import Particle
import MC

type SourceIntensity = FPFloat

class Source a where
    sampleParticles :: a -> MC [Particle]

data (SpaceDistribution a, MomentumDistribution b) => FactorizedSource a b =
    FactorizedSource { spaceDistribution    :: a
                     , momentumDistribution :: b
                     } deriving (Show, Eq)

class SpaceDistribution a where
    samplePosition :: a -> MC Position

class MomentumDistribution a where
    sampleMomentum :: a -> MC Momentum

newtype PointwiseSpaceDistribution =
    PointwiseSpaceDistribution { _center :: Position } deriving (Show, Eq)

instance SpaceDistribution PointwiseSpaceDistribution where
    samplePosition source = return $ _center source

newtype IsoMonoDistribution =
    IsoMonoDistribution { _momentumValue :: FPFloat } deriving (Show, Eq)

instance MomentumDistribution IsoMonoDistribution where
    sampleMomentum source = Mom <$> sampleIsoVec (_momentumValue source)

type PointwiseIsoMonoSource = FactorizedSource PointwiseSpaceDistribution IsoMonoDistribution

mkPWIMSource :: Position -> FPFloat -> PointwiseIsoMonoSource
mkPWIMSource pos mom = FactorizedSource (PointwiseSpaceDistribution pos) (IsoMonoDistribution mom)

makeLenses ''PointwiseSpaceDistribution
makeLenses ''IsoMonoDistribution

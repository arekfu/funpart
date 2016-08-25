{-# LANGUAGE TemplateHaskell #-}

module Source.Distributions
( SpaceDistribution(..)
, center
, MomentumDistribution(..)
, momentumValue
, PointwiseSpaceDistribution(..)
, IsoMonoDistribution(..)
) where

import Control.Lens

import Core
import MC
import Particle

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

makeLenses ''PointwiseSpaceDistribution
makeLenses ''IsoMonoDistribution

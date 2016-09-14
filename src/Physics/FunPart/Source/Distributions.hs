{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Physics.FunPart.Source.Distributions
( SpaceDistribution(..)
, center
, MomentumDistribution(..)
, momentumValue
, PointwiseSpaceDistribution(..)
, IsoMonoDistribution(..)
) where

import Control.Lens
import Control.Monad.State.Class (MonadState)
import System.Random (StdGen)

import Physics.FunPart.Core
import Physics.FunPart.Particle
import Physics.FunPart.MC (sampleIsoVec)

class SpaceDistribution a where
    samplePosition :: MonadState StdGen m => a -> m Position

class MomentumDistribution a where
    sampleMomentum :: MonadState StdGen m => a -> m Momentum

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

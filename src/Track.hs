{-# LANGUAGE TemplateHaskell #-} -- for makeLenses

module Track
( TrackPointType(..)
, TrackPoint(..)
, pointType
, pointVertex
, pointMomentum
, pointWeight
, Track(..)
, trackPoints
) where

import Control.Lens

import Particle
import CrossSection

data TrackPointType = CollisionPoint CrossSection
                    | SourcePoint
                    | EndPoint

data TrackPoint = TrackPoint { _pointType     :: TrackPointType
                             , _pointVertex   :: Position
                             , _pointMomentum :: Momentum
                             , _pointWeight   :: Weight
                             }
makeLenses ''TrackPoint

newtype Track = Track { _trackPoints :: [TrackPoint] }
makeLenses ''Track

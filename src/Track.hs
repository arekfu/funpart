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

import Control.Lens (makeLenses)

import Particle
import CrossSection

data TrackPointType = CollisionPoint CrossSection [Track]
                    | SourcePoint
                    | EndPoint

data TrackPoint = TrackPoint { _pointType     :: TrackPointType
                             , _pointVertex   :: Position
                             , _pointMomentum :: Momentum
                             , _pointWeight   :: Weight
                             }

newtype Track = Track { _trackPoints :: [TrackPoint] }

makeLenses ''TrackPoint
makeLenses ''Track

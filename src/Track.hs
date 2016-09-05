{-# LANGUAGE TemplateHaskell #-} -- for makeLenses
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Track
( TrackPointType(..)
, collisionXSec
, secondaries
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

data TrackPointType = CollisionPoint { _collisionXSec :: !CrossSectionValue
                                     , _secondaries :: ![Track]
                                     }
                    | SourcePoint
                    | SecondaryPoint
                    | EndPoint
                    deriving (Show, Eq)

data TrackPoint = TrackPoint { _pointType     :: !TrackPointType
                             , _pointVertex   :: !Position
                             , _pointMomentum :: !Momentum
                             , _pointWeight   :: !Weight
                             } deriving (Show, Eq)

newtype Track = Track { _trackPoints :: [TrackPoint] } deriving (Show, Eq)

makeLenses ''TrackPointType
makeLenses ''TrackPoint
makeLenses ''Track

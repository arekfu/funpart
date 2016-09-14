{-# LANGUAGE TemplateHaskell #-} -- for makeLenses
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Physics.FunPart.Track
( Collision(..)
, primary
, secondaries
, TrackPointType(..)
, collisionXSec
, collision
, TrackPoint(..)
, pointType
, pointVertex
, pointMomentum
, pointWeight
, Track(..)
, trackPoints
) where

import Control.Lens (makeLenses)
import Data.Sequence (Seq)

import Physics.FunPart.Particle
import Physics.FunPart.CrossSection

data Collision = Elastic { _primary :: !Particle }
               | Inelastic { _secondaries :: ![Track] }
               deriving (Show, Eq)

data TrackPointType = CollisionPoint { _collisionXSec :: !CrossSectionValue
                                     , _collision :: !Collision
                                     }
                    | SourcePoint
                    | SecondaryPoint
                    | Absorption
                    deriving (Show, Eq)

data TrackPoint = TrackPoint { _pointType     :: !TrackPointType
                             , _pointVertex   :: !Position
                             , _pointMomentum :: !Momentum
                             , _pointWeight   :: !Weight
                             } deriving (Show, Eq)

newtype Track = Track { _trackPoints :: Seq TrackPoint } deriving (Show, Eq)

makeLenses ''Collision
makeLenses ''TrackPointType
makeLenses ''TrackPoint
makeLenses ''Track

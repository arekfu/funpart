{-# LANGUAGE TemplateHaskell #-} -- for makeLenses

module Step
( StepPointType(..)
, stepCollisionXSec
, outgoing
, StepPoint(..)
, stepPointType
, stepPointVertex
, stepPointMomentum
, stepPointWeight
, getOutgoing
, toTrackPoint
) where

import Control.Lens (makeLenses)

import Particle
import CrossSection
import Track

data StepPointType = CollisionStepPoint { _stepCollisionXSec :: CrossSectionValue
                                        , _outgoing          :: [Particle]
                                        }
                   | SourceStepPoint
                   | EndStepPoint
                   deriving (Show, Eq)

toTrackPointType :: StepPointType -> TrackPointType
toTrackPointType SourceStepPoint = SourcePoint
toTrackPointType EndStepPoint = EndPoint
toTrackPointType (CollisionStepPoint xs _) = CollisionPoint xs

getOutgoing :: StepPoint -> [Particle]
getOutgoing (StepPoint (CollisionStepPoint _ ps) _ _ _) = ps
getOutgoing _ = []

data StepPoint = StepPoint { _stepPointType     :: StepPointType
                           , _stepPointVertex   :: Position
                           , _stepPointMomentum :: Momentum
                           , _stepPointWeight   :: Weight
                           } deriving (Show, Eq)

toTrackPoint :: StepPoint -> TrackPoint
toTrackPoint (StepPoint typ pos mom wei) = TrackPoint (toTrackPointType typ)
                                                      pos
                                                      mom
                                                      wei


makeLenses ''StepPointType
makeLenses ''StepPoint

{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Particle
( mkParticle
, mkDynParticle
, position
, momentum
, positionVec
, momentumVec
, ptype
, dynPart
, pPosition
, pMomentum
, pPositionVec
, pMomentumVec
, ParticleType(..)
, Position(..)
, Momentum(..)
, push
, Particle(..)
, DynParticle(..)
, Distance
) where

import Vec
import VecSpace
import Approx
import Control.Lens

newtype Position = Pos { _positionVec :: FPVec3 }
    deriving (Show, Eq, VecSpace FPFloat, Approx)
makeLenses ''Position

newtype Momentum = Mom { _momentumVec :: FPVec3 }
    deriving (Show, Eq, VecSpace FPFloat, Approx)
makeLenses ''Momentum

data ParticleType = Neutron
                  | Photon
                  deriving (Show, Eq)

data DynParticle = DP { _position :: Position
                      , _momentum :: Momentum
                      } deriving (Show, Eq)
makeLenses ''DynParticle

data Particle = P { _ptype   :: ParticleType
                  , _dynPart :: DynParticle
                  } deriving (Show, Eq)
makeLenses ''Particle

pPosition :: Lens' Particle Position
pPosition = dynPart.position

pMomentum :: Lens' Particle Momentum
pMomentum = dynPart.momentum

pPositionVec :: Lens' Particle FPVec3
pPositionVec = pPosition.positionVec

pMomentumVec :: Lens' Particle FPVec3
pMomentumVec = pMomentum.momentumVec

mkDynParticle :: Position -> Momentum -> DynParticle
mkDynParticle = DP

mkParticle :: ParticleType -> DynParticle -> Particle
mkParticle = P

type Distance = FPFloat
push :: Distance -> DynParticle -> Maybe DynParticle
push dist (DP (Pos r) (Mom p)) = do pHat <- toUnitVector p
                                    let r' = r +: dist *: pHat
                                     in return $ DP (Pos r') (Mom p)

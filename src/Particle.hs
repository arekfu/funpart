{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Particle
( mkParticle
, mkDynParticle
, ParticleType(..)
, Weight
, Position(..)
, positionVec
, Momentum(..)
, momentumVec
, DynParticle(..)
, position
, momentum
, weight
, Particle(..)
, ptype
, dynPart
, pPosition
, pMomentum
, pPositionVec
, pMomentumVec
, Distance
, Pushable
, push
) where

import Core
import Vec
import VecSpace
import Approx
import Control.Lens

newtype Position = Pos { _positionVec :: FPVec3 }
    deriving (Show, Eq, VecSpace FPFloat, Approx)

newtype Momentum = Mom { _momentumVec :: FPVec3 }
    deriving (Show, Eq, VecSpace FPFloat, Approx)

data ParticleType = Neutron
                  | Photon
                  deriving (Show, Eq)

type Weight = FPFloat

data DynParticle = DP { _position :: Position
                      , _momentum :: Momentum
                      , _weight   :: Weight
                      } deriving (Show, Eq)

data Particle = P { _ptype   :: ParticleType
                  , _dynPart :: DynParticle
                  } deriving (Show, Eq)

makeLenses ''Position
makeLenses ''Momentum
makeLenses ''DynParticle
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
mkDynParticle r p = DP r p 1.0

mkParticle :: ParticleType -> DynParticle -> Particle
mkParticle = P

type Distance = FPFloat

class Pushable a where
    push :: Distance -> a -> Maybe a

instance Pushable DynParticle where
    push dist (DP (Pos r) (Mom p) w) = do pHat <- toUnitVector p
                                          let r' = r +: dist *: pHat
                                          return $ DP (Pos r') (Mom p) w

instance Pushable Particle where
    push dist (P pt dp) = do
                            dp' <- push dist dp
                            return $ P pt dp'

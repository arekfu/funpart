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
, Pushable
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

class Pushable a where
    push :: Distance -> a -> Maybe a

instance Pushable DynParticle where
    push dist (DP (Pos r) (Mom p)) = do pHat <- toUnitVector p
                                        let r' = r +: dist *: pHat
                                        return $ DP (Pos r') (Mom p)

instance Pushable Particle where
    push dist (P pt dp) = do
                            dp' <- push dist dp
                            return $ P pt dp'

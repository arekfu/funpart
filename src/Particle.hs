{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Particle
( mkParticle
, ptype
, position
, momentum
, ParticleType(..)
, push
, Particle()
, DynParticle()
) where

import Core
import VecSpace

newtype Position = Position FPVec3
    deriving (Show, Eq, VecSpace FPFloat)

newtype Momentum = Mom FPVec3
    deriving (Show, Eq, VecSpace FPFloat)

data ParticleType = Neutron
                  | Photon

data DynParticle = DP { pos :: Position
                      , mom :: Momentum
                      }

data Particle = P { ptype :: ParticleType
                  , dynPart :: DynParticle
                  }

position :: Particle -> Position
position = pos . dynPart

momentum :: Particle -> Momentum
momentum = mom . dynPart

mkDynParticle :: Position -> Momentum -> DynParticle
mkDynParticle = DP

mkParticle :: ParticleType -> Position -> Momentum -> Particle
mkParticle type_ r p = P type_ $ mkDynParticle r p

type Distance = FPFloat
push :: DynParticle -> Distance -> DynParticle
push (DP r p) d = DP ((1.0 + d) *: r) p

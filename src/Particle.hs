{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Particle
( mkParticle
, mkDynParticle
, position
, momentum
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

newtype Position = Pos { getPos :: FPVec3 }
    deriving (Show, Eq, VecSpace FPFloat, Approx)

newtype Momentum = Mom { getMom :: FPVec3 }
    deriving (Show, Eq, VecSpace FPFloat, Approx)

data ParticleType = Neutron
                  | Photon
                  deriving (Show, Eq)

data DynParticle = DP { pos :: Position
                      , mom :: Momentum
                      } deriving (Show, Eq)

data Particle = P { ptype   :: ParticleType
                  , dynPart :: DynParticle
                  } deriving (Show, Eq)

position :: Particle -> Position
position = pos . dynPart

momentum :: Particle -> Momentum
momentum = mom . dynPart

mkDynParticle :: Position -> Momentum -> DynParticle
mkDynParticle = DP

mkParticle :: ParticleType -> DynParticle -> Particle
mkParticle = P

type Distance = FPFloat
push :: Distance -> DynParticle -> Maybe DynParticle
push dist (DP (Pos r) (Mom p)) = do pHat <- toUnitVector p
                                    let r' = r +: dist *: pHat
                                     in return $ DP (Pos r') (Mom p)

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

import Vec
import VecSpace

newtype Position = Pos FPVec3
    deriving (Show, Eq, VecSpace FPFloat)

newtype Momentum = Mom FPVec3
    deriving (Show, Eq, VecSpace FPFloat)

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

mkParticle :: ParticleType -> Position -> Momentum -> Particle
mkParticle type_ r p = P type_ $ mkDynParticle r p

type Distance = FPFloat
push :: Distance -> DynParticle -> Maybe DynParticle
push dist (DP (Pos r) (Mom p)) = do pHat <- toUnitVector p
                                    let r' = r +: dist *: pHat
                                     in return $ DP (Pos r') (Mom p)

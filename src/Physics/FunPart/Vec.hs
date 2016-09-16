{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Physics.FunPart.Vec
( Vec3(..)
, FPVec3
, normalize
, toUnitVector
, iHat
, jHat
, kHat
, VecSpace(..)
, EuclidVecSpace(..)
) where

import Physics.FunPart.Core
import Physics.FunPart.VecSpace
import Physics.FunPart.Approx

data Vec3 a = Vec3 !a !a !a deriving Eq

type FPVec3 = Vec3 FPFloat

-----------------
--  instances  --
-----------------

instance Show a => Show (Vec3 a) where
    show (Vec3 x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Num a => VecSpace a (Vec3 a) where
    (Vec3 x0 y0 z0) +: (Vec3 x1 y1 z1) = Vec3 (x0+x1) (y0+y1) (z0+z1)
    (Vec3 x0 y0 z0) -: (Vec3 x1 y1 z1) = Vec3 (x0-x1) (y0-y1) (z0-z1)
    s *: (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)
    zero = Vec3 0 0 0

instance Num a => Monoid (Vec3 a) where
    mempty = zero
    mappend = (+:)

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Num a => EuclidVecSpace a (Vec3 a) where
    (Vec3 x0 y0 z0) .: (Vec3 x1 y1 z1) = x0*x1 + y0*y1 + z0*z1

instance (Approx a, Real a, Floating a) => Approx (Vec3 a) where
    distance v1 v2 = realToFrac $ mag $ v1 -: v2
    (Vec3 x0 y0 z0) ~== (Vec3 x1 y1 z1) = x0~==x1 && y0~==y1 && z0~==z1

normalize :: FPFloat -> FPVec3 -> Maybe FPVec3
normalize newNorm v = let norm = mag v
                       in if norm > 0
                          then Just $ (newNorm/norm) *: v
                          else Nothing

toUnitVector :: FPVec3 -> Maybe FPVec3
toUnitVector = normalize 1.0

-- | x-axis unit vector
iHat :: Num a => Vec3 a
iHat = Vec3 1 0 0

-- | y-axis unit vector
jHat :: Num a => Vec3 a
jHat = Vec3 0 1 0

-- | z-axis unit vector
kHat :: Num a => Vec3 a
kHat = Vec3 0 0 1

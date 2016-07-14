{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Vec
( cart
, Vec3()
, DVec3()
, normalize
, iHat
, jHat
, kHat
) where

import VecSpace
import Approx

data Vec3 a = Vec3 a a a deriving Eq

type DVec3 = Vec3 Double

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

instance Num a => EuclidVecSpace a (Vec3 a) where
    (Vec3 x0 y0 z0) .: (Vec3 x1 y1 z1) = x0*x1 + y0*y1 + z0*z1

instance (Approx a, Real a, Floating a) => Approx (Vec3 a) where
    distance v1 v2 = realToFrac $ mag $ v1 -: v2
    (Vec3 x0 y0 z0) ~== (Vec3 x1 y1 z1) = x0~==x1 && y0~==y1 && z0~==z1

normalize :: Double -> DVec3 -> Maybe DVec3
normalize newNorm v = let norm = mag v
                       in if norm > 0
                          then Just $ (newNorm/norm) *: v
                          else Nothing

-- | the actual value constructor
cart :: a -> a -> a -> Vec3 a
cart = Vec3

-- | x-axis unit vector
iHat :: Num a => Vec3 a
iHat = Vec3 1 0 0

-- | y-axis unit vector
jHat :: Num a => Vec3 a
jHat = Vec3 0 1 0

-- | z-axis unit vector
kHat :: Num a => Vec3 a
kHat = Vec3 0 0 1

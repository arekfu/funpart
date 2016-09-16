{-# LANGUAGE RankNTypes, GADTs #-}

module Physics.FunPart.Coords
( Coord(..)
, CoordTransform(..)
, cartesianTransform
, sphericalTransform
, cylindricalTransform
) where

import Physics.FunPart.Vec


class Coord a where
    getU :: a b -> b
    getV :: a b -> b
    getW :: a b -> b

-- | Datatype representing a point in cartesian coordinates.
data CartesianCoord a = CartesianCoord a a a

cartesianToVec :: CartesianCoord a -> Vec3 a
cartesianToVec (CartesianCoord x y z) = Vec3 x y z

vecToCartesian :: Vec3 a -> CartesianCoord a
vecToCartesian (Vec3 x y z) = CartesianCoord x y z

instance Coord CartesianCoord where
    getU (CartesianCoord x _ _) = x
    getV (CartesianCoord _ y _) = y
    getW (CartesianCoord _ _ z) = z

-- | Datatype representing a point in spherical coordinates.
data SphericalCoord a = SphericalCoord a a a

sphericalToVec :: RealFloat a => SphericalCoord a -> Vec3 a
sphericalToVec (SphericalCoord r θ φ) = Vec3 x y z
        where x = r * sin θ * cos φ
              y = r * sin θ * sin φ
              z = r * cos θ

instance Coord SphericalCoord where
    getU (SphericalCoord r _ _) = r
    getV (SphericalCoord _ θ _) = θ
    getW (SphericalCoord _ _ φ) = φ

vecToSpherical :: RealFloat a => Vec3 a -> SphericalCoord a
vecToSpherical (Vec3 x y z) = SphericalCoord r θ φ
    where ρ2 = x^(2::Int) + y^(2::Int)
          r = sqrt $ ρ2 + z^(2::Int)
          θ = atan2 (sqrt ρ2) z
          φ = atan2 y x


-- | Datatype representing a point in cylindrical coordinates.
data CylindricalCoord a = CylindricalCoord a a a

instance Coord CylindricalCoord where
    getU (CylindricalCoord ρ _ _) = ρ
    getV (CylindricalCoord _ φ _) = φ
    getW (CylindricalCoord _ _ z) = z

cylindricalToVec :: RealFloat a => CylindricalCoord a -> Vec3 a
cylindricalToVec (CylindricalCoord ρ φ z) = Vec3 x y z
    where x = ρ * cos φ
          y = ρ * sin φ

vecToCylindrical :: RealFloat a => Vec3 a -> CylindricalCoord a
vecToCylindrical (Vec3 x y z) = CylindricalCoord ρ φ z
    where ρ = sqrt $ x^(2::Int) + y^(2::Int)
          φ = atan2 y x


data CoordTransform a b = CoordTransform { transform        :: Vec3 b -> a b
                                         , inverseTransform :: a b -> Vec3 b
                                         }

cartesianTransform :: RealFloat a => CoordTransform CartesianCoord a
cartesianTransform = CoordTransform vecToCartesian cartesianToVec

sphericalTransform :: RealFloat a => CoordTransform SphericalCoord a
sphericalTransform = CoordTransform vecToSpherical sphericalToVec

cylindricalTransform :: RealFloat a => CoordTransform CylindricalCoord a
cylindricalTransform = CoordTransform vecToCylindrical cylindricalToVec

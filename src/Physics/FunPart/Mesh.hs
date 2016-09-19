-- |
-- Module      :  Physics.FunPart.Mesh
-- Copyright   :  Davide Mancusi 2016
-- License     :  BSD3
--
-- Maintainer  :  arekfu@yahoo.it
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module introduces datatypes that describe structured meshes and fields
-- on the meshes.
--

{-# LANGUAGE GADTs #-}

module Physics.FunPart.Mesh
( InterpolationType(..)
, axisSpec
, AxisSpec
, findOnAxis
, MeshSpec(..)
, meshBins
, findInMeshCoords
, toSeqIndex
, maybeToSeqIndex
, fromSeqIndex
, maybefromSeqIndex
, Mesh
, cartesianMesh
, sphericalMesh
, cylindricalMesh
, linMeshSpec
, Field
, field
, fieldFrom
) where

import qualified Data.Vector.Unboxed as V

import Physics.FunPart.Coords
import Physics.FunPart.Core
import Physics.FunPart.Vec

-- | An ADT for interpolation types -- currently limited to linear and
-- logarithmic interpolation.
data InterpolationType = LinInterpolation
                       | LogInterpolation
                       deriving (Show, Eq)

-- | Wrapper for mesh coordinates.
newtype MeshCoord = MeshC FPFloat deriving (Show, Eq)

-- | Specifies binning along an axis. The fields are:
--
-- * Interpolation type (duh);
-- * Precalculated minimum value (accounts for the interpolation type);
-- * Precalculated width (accounts for the interpolation type);
-- * Number of bins.
data AxisSpec = AxisSpec !InterpolationType
                         {-# UNPACK #-} !MeshCoord
                         {-# UNPACK #-} !MeshCoord
                         {-# UNPACK #-} !Int
                         deriving (Show, Eq)

-- | Getter for the number of bins.
nBins :: AxisSpec -> Int
nBins (AxisSpec _ _ _ n) = n

-- | Construct an axis specification.
axisSpec :: InterpolationType   -- ^ The interpolation type.
         -> FPFloat             -- ^ The axis minimum.
         -> FPFloat             -- ^ The axis maximum.
         -> Int                 -- ^ The number of bins.
         -> AxisSpec            -- ^ The resulting axis specification.
axisSpec typ a0 a1 = AxisSpec typ (MeshC axMin) (MeshC axWidth)
    where axWidth = case typ of
                        LinInterpolation -> a1 - axMin
                        LogInterpolation -> log a1 - axMin
          axMin = case typ of
                      LinInterpolation -> a0
                      LogInterpolation -> log a0

-- | Find the 0-based index of the bin containing the given coordinate value.
findOnAxis :: AxisSpec -> FPFloat -> Maybe Int
findOnAxis (AxisSpec typ (MeshC a0) (MeshC aw) n) a =
    if i<0 || i>=n then Nothing else Just i
    where i = case typ of
                  LinInterpolation -> floor $ ((a-a0) * fromIntegral n)/aw
                  LogInterpolation -> floor $ ((log a-a0) * fromIntegral n)/aw


-- | A mesh specification.
data MeshSpec = MeshSpec {-# UNPACK #-} !AxisSpec
                         {-# UNPACK #-} !AxisSpec
                         {-# UNPACK #-} !AxisSpec
                         deriving (Show, Eq)

linMeshSpec :: (FPFloat, FPFloat, FPFloat) -> (FPFloat, FPFloat, FPFloat) -> (Int, Int, Int) -> MeshSpec
linMeshSpec (u0, v0, w0) (u1, v1, w1) (nu, nv, nw) = MeshSpec uAx vAx wAx
    where uAx = axisSpec LinInterpolation u0 u1 nu
          vAx = axisSpec LinInterpolation v0 v1 nv
          wAx = axisSpec LinInterpolation w0 w1 nw

-- | Returns the number of bins in the mesh.
meshBins :: MeshSpec -> Int
meshBins (MeshSpec uAx vAx wAx) = nBins uAx * nBins vAx * nBins wAx

-- | Find the triplet of axis indices, given a mesh specification and a triplet
-- of mesh coordinates (that is, in the UVW frame).
findInMeshCoords :: MeshSpec -> FPFloat -> FPFloat -> FPFloat -> Maybe (Int, Int, Int)
findInMeshCoords (MeshSpec uAx vAx wAx) u v w = do iu <- findOnAxis uAx u
                                                   iv <- findOnAxis vAx v
                                                   iw <- findOnAxis wAx w
                                                   return (iu, iv, iw)

-- | Transform a triplet of mesh indices in a one-dimensional index, suitable
-- for indexing in a flat, one-dimensional representation of the mesh values.
toSeqIndex :: MeshSpec -> (Int, Int, Int) -> Int
toSeqIndex (MeshSpec uAx vAx _) (iu, iv, iw) = iw + nv * (iv + nu * iu)
    where nu = nBins uAx
          nv = nBins vAx

-- | Safe version of 'toSeqIndex'.
maybeToSeqIndex :: MeshSpec -> (Int, Int, Int) -> Maybe Int
maybeToSeqIndex spec indices =
    if i<0 || i>=n then Nothing else Just i
    where i = toSeqIndex spec indices
          n = meshBins spec

-- | Transform a triplet of mesh indices in a one-dimensional index, suitable
-- for indexing in a flat, one-dimensional representation of the mesh values.
fromSeqIndex :: MeshSpec -> Int -> (Int, Int, Int)
fromSeqIndex (MeshSpec uAx vAx _) i = (iu, iv, iw)
    where nu = nBins uAx
          nv = nBins vAx
          iw = i `mod` nv
          jv = i `div` nv
          iv = jv `mod` nu
          iu = jv `div` nu

-- | Safe version of 'fromSeqIndex'.
maybefromSeqIndex :: MeshSpec -> Int -> Maybe (Int, Int, Int)
maybefromSeqIndex spec@(MeshSpec uAx vAx wAx) i =
    if iu<0 || iu>=nu || iv<0 || iv>=nv || iw<0 || iw>=nw then Nothing else Just trip
    where trip@(iu, iv, iw) = fromSeqIndex spec i
          nu = nBins uAx
          nv = nBins vAx
          nw = nBins wAx


-- | The full description of a mesh. The 'FPVec3' parameter specifies the
-- origin for the positioning of the mesh.
data Mesh where
    Mesh :: (Coord a, RealFloat b)
         => CoordTransform a b  -- ^ The coordinate system.
         -> MeshSpec            -- ^ The mesh specification.
         -> Vec3 b              -- ^ The mesh origin offset.
         {- -> RotationMatrix? -}
         -> Mesh                -- ^ The resulting Mesh.

cartesianMesh :: RealFloat a => MeshSpec -> Vec3 a -> Mesh
cartesianMesh = Mesh cartesianTransform

sphericalMesh :: RealFloat a => MeshSpec -> Vec3 a -> Mesh
sphericalMesh = Mesh sphericalTransform

cylindricalMesh :: RealFloat a => MeshSpec -> Vec3 a -> Mesh
cylindricalMesh = Mesh cylindricalTransform


-- | A field carrying values of type 'a' defined on a mesh. The values are
-- internally stored in a flat one-dimensional vector.
data Field a = Field Mesh (V.Vector a)

-- | Construct a zero field on the given mesh.
field :: (Num a, V.Unbox a) => Mesh -> Field a
field mesh@(Mesh _ spec _) = Field mesh $ V.replicate (meshBins spec) 0

-- | Construct a field on the given mesh given a value constructor. The
-- constructor will be passed the mesh indices of the cell to initialize.
fieldFrom :: (Num a, V.Unbox a) => Mesh -> ((Int, Int, Int) -> a) -> Field a
fieldFrom mesh@(Mesh _ spec _) constructor =
    Field mesh $ V.generate (meshBins spec) generator
    where generator = constructor . fromSeqIndex spec

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module VecTest
( runTests
) where

import Test.QuickCheck
import Control.Monad (liftM3)

import Approx
import Vec
import VecSpace

newtype (Approx a) => AVec3 a = AVec3 (Vec3 a)
    deriving (VecSpace a, EuclidVecSpace a, Show, Eq)

instance (Approx a, Arbitrary a) => Arbitrary (AVec3 a) where
    arbitrary = AVec3 <$> liftM3 cart arbitrary arbitrary arbitrary

instance (Approx a, Real a, Floating a) => Approx (AVec3 a) where
    distance (AVec3 v1) (AVec3 v2) = distance v1 v2

prop_linearityInt :: Int -> AVec3 Int -> AVec3 Int -> Bool
prop_linearityInt x v1 v2 = (x *: v1) +: (x *: v2) == x *: (v1 +: v2)

prop_linearityDouble :: Double -> AVec3 Double -> AVec3 Double -> Bool
prop_linearityDouble x v1 v2 = (x *: v1) +: (x *: v2) ~== x *: (v1 +: v2)

prop_Schwartz :: AVec3 Double -> AVec3 Double -> Bool
prop_Schwartz v1 v2 = let sum_ = v1 +: v2
                          diff = v1 -: v2
                          norm1 = mag v1
                          norm2 = mag v2
                       in mag sum_ <= norm1 + norm2 && mag diff >= abs (norm1 - norm2)

return []
runTests :: IO Bool
runTests = $quickCheckAll

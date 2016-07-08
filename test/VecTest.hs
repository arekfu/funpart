{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module VecTest
( runTests
) where

import Test.QuickCheck

import Vec
import VecSpace

newtype AVec3 a = AVec3 (Vec3 a) deriving (VecSpace a, Show, Eq)

instance Arbitrary a => Arbitrary (AVec3 a) where
    arbitrary = do
                    x <- arbitrary
                    y <- arbitrary
                    z <- arbitrary
                    return $ AVec3 $ cart x y z

prop_linearity :: Int -> AVec3 Int -> AVec3 Int -> Bool
prop_linearity x v1 v2 = (x *: v1) +: (x *: v2) == x *: (v1 +: v2)

return []
runTests :: IO Bool
runTests = $quickCheckAll

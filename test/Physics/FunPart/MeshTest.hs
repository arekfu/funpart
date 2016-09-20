{-# LANGUAGE TemplateHaskell #-}

module Physics.FunPart.MeshTest
( runTests
) where

import Test.QuickCheck

import Physics.FunPart.Mesh

newtype AInterpolationType = AInterpolationType InterpolationType deriving (Show, Eq)

instance Arbitrary AInterpolationType where
    arbitrary = AInterpolationType <$> elements [LinInterpolation, LogInterpolation]


newtype AAxisSpec a = AAxisSpec (AxisSpec a) deriving (Show, Eq)

instance (Arbitrary a, RealFloat a) => Arbitrary (AAxisSpec a) where
    arbitrary = do (AInterpolationType iType) <- arbitrary :: Gen AInterpolationType
                   a1 <- arbitrary
                   a2 <- arbitrary
                   let aMin = min a1 a2
                   let aMax = max a1 a2
                   n <- arbitrary
                   return $ AAxisSpec $ axisSpec iType aMin aMax n


return []
runTests :: IO Bool
runTests = $quickCheckAll

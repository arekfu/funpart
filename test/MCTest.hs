{-# LANGUAGE TemplateHaskell #-}

module MCTest
( runTests
) where

import qualified Data.Vector as V
import Test.QuickCheck
import Data.Maybe (isNothing)
import System.Random (mkStdGen)

import Core
import Approx
import MC
import Vec

newtype PositiveVector a = PositiveVector (V.Vector a)
                           deriving (Show, Eq, Ord)

instance (Arbitrary a, Ord a, Num a, Fractional a) => Arbitrary (PositiveVector a) where
  arbitrary = let helper = do n <- choose (1, 20)
                              v <- vector n
                              return $ fromVector $ V.map abs $ V.fromList v
               in helper `suchThat` anyPositive

anyPositive :: (Ord a, Num a) => PositiveVector a -> Bool
anyPositive (PositiveVector v) = V.any (>0) v

fromVector :: (Ord a, Num a, Fractional a) => V.Vector a -> PositiveVector a
fromVector v = let tot = V.sum v
                in PositiveVector $ fmap (/tot) v

prop_getFirst :: PositiveVector Double -> Bool
prop_getFirst (PositiveVector v) = let i = sampleV v 0.0
                                    in i == Just 1

prop_getLast :: PositiveVector Double -> Bool
prop_getLast (PositiveVector v) = let i = sampleV v 1.1
                                   in isNothing i

prop_normalisation :: Positive FPFloat -> Positive (Large Int) -> Bool
prop_normalisation norm seed = mag randomVec ~== norm'
    where norm' = getPositive norm
          seed' = getLarge $ getPositive seed
          randomVec = evalMC (sampleIsoVec norm') $ mkStdGen seed'

return []
runTests :: IO Bool
runTests = $quickCheckAll

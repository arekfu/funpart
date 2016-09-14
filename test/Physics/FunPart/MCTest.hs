{-# LANGUAGE TemplateHaskell #-}

module Physics.FunPart.MCTest
( runTests
) where

import qualified Data.Vector as V
import Test.QuickCheck
import Data.Maybe (isNothing)
import System.Random (mkStdGen)
import Control.Monad.State (evalState)

import Physics.FunPart.Core
import Physics.FunPart.Approx
import Physics.FunPart.MC
import Physics.FunPart.Vec

newtype PositiveVector a = PositiveVector (V.Vector a)
                           deriving (Show, Eq, Ord)

instance (Arbitrary a, Ord a, Num a, Fractional a) => Arbitrary (PositiveVector a) where
  arbitrary = let helper = do n <- choose (1, 20)
                              v <- vector n
                              return $ fromVector $ V.map abs $ V.fromList v
               in helper `suchThat` anyPositive

anyPositive :: (Ord a, Num a) => PositiveVector a -> Bool
anyPositive (PositiveVector v) = V.any (>0) v

fromVector :: Fractional a => V.Vector a -> PositiveVector a
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
          randomVec = evalState (sampleIsoVec norm') $ mkStdGen seed'

prop_positiveExp :: Positive FPFloat -> Positive (Large Int) -> Bool
prop_positiveExp lambda seed = expRandom >= 0.0
    where expRandom = evalState (sampleExp lambda') $ mkStdGen seed'
          lambda' = getPositive lambda
          seed' = getLarge $ getPositive seed

return []
runTests :: IO Bool
runTests = $quickCheckAll

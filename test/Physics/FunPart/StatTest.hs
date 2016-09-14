{-# LANGUAGE TemplateHaskell #-}

module Physics.FunPart.StatTest
( runTests
) where

import Test.QuickCheck
import Data.Foldable (foldl')
import Data.Maybe (isNothing, fromJust)

import Physics.FunPart.Approx
import Physics.FunPart.Stat

naiveMean :: [Double] -> Double
naiveMean l = sum l / fromIntegral (length l)

prop_meanCorrect :: [Double] -> Property
prop_meanCorrect l = not (null l) ==> sVarMean ~== naiveMean l
    where sVarMean = mean $ foldl' tally empty l

prop_rmsCorrect :: [Double] -> Property
prop_rmsCorrect l =
    counterexample (show (rms sVar) ++ "; " ++ show naiveRMS ++ "\n" ++ show sVar ++ "\n" ++ show residues) $
    if length l > 1
    then fromJust (rms sVar) ~== naiveRMS
    else isNothing $ rms sVar
    where sVar = foldl' tally empty l
          xbar = naiveMean l
          residues = map (\x -> (x-xbar)^(2::Int)) l
          len = fromIntegral (length l)
          naiveRMS = sqrt $ naiveMean residues / (len - 1)

flatten :: [(Double, Positive Int)] -> [Double]
flatten = concatMap (\(x,Positive n) -> replicate n x)

flattenSVar :: [(Double, Positive Int)] -> SVar Double
flattenSVar l = foldl' tally empty $ flatten l

prop_weightedMeanCorrect :: [(Double, Positive Int)] -> Property
prop_weightedMeanCorrect l =
    not (null l) ==> mean sVar ~== mean flatSVar
    where flatSVar = flattenSVar l
          sVar = foldl' (\sv (x, Positive n) -> tallyW sv x (fromIntegral n)) empty l

-- | Assert that the mean is invariant if all the weights are equal.
prop_homogeneousMean :: [Double] -> Double -> Property
prop_homogeneousMean l w = not (null l) ==> mean_ ~== scaledMean
    where sVar = foldl' tally empty l
          mean_ = mean sVar
          scaledSVar = foldl' (\sv x -> tallyW sv x w) empty l
          scaledMean = mean scaledSVar

-- | Assert that the RMS is invariant if all the weights are equal.
prop_homogeneousRMS :: [Double] -> Double -> Property
prop_homogeneousRMS l w = not (null l) ==> rms_ ~== scaledRMS
    where sVar = foldl' tally empty l
          rms_ = rms sVar
          scaledSVar = foldl' (\sv x -> tallyW sv x w) empty l
          scaledRMS = rms scaledSVar

--prop_weightedRMSCorrect :: [(Double, Positive Int)] -> Property
--prop_weightedRMSCorrect l =
--    counterexample (show (rms sVar) ++ "; " ++ show (rms flatSVar) ++ "\n" ++ show sVar ++ "\n" ++ show flatSVar) $
--    if length l > 1
--    then fromJust (rms sVar) ~== fromJust (rms flatSVar)
--    else isNothing $ rms sVar
--    where flatSVar = flattenSVar l
--          sVar = foldl' (\sv (x, Positive n) -> tallyW sv x (fromIntegral n)) empty l

return []
runTests :: IO Bool
runTests = $quickCheckAll

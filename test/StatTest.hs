{-# LANGUAGE TemplateHaskell #-}

module StatTest
( runTests
) where

import Test.QuickCheck
import Data.Foldable (foldl')
import Data.Maybe (fromJust)

import Approx
import Stat

naiveMean :: [Double] -> Double
naiveMean l = sum l / fromIntegral (length l)

prop_meanCorrect :: [Double] -> Property
prop_meanCorrect l = not (null l) ==> svarMean ~== naiveMean l
    where svarMean = mean $ foldl' score empty l

prop_rmsCorrect :: [Double] -> Property
prop_rmsCorrect l =
    counterexample (show (rms svar) ++ "; " ++ show naiveRMS ++ "\n" ++ show svar ++ "\n" ++ show residues) $
    length l > 1 ==> fromJust (rms svar) ~== naiveRMS
    where svar = foldl' score empty l
          xbar = naiveMean l
          residues = map (\x -> (x-xbar)^(2::Int)) l
          len = fromIntegral (length l)
          naiveRMS = sqrt $ naiveMean residues * len / (len - 1)

return []
runTests :: IO Bool
runTests = $quickCheckAll

module Approx
( (~==)
, distance
, Approx
) where

tolerance :: Double
tolerance = 1E-6

class Approx a where
    distance :: a -> a -> Double
    (~==) :: a -> a -> Bool
    x~==y = abs (distance x y) <= tolerance
    infix 4 ~==

instance Approx Double where
    distance x y = x-y

instance Approx Int where
    distance x y = fromIntegral (x-y)

instance Approx Integer where
    distance x y = fromIntegral (x-y)

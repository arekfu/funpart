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

instance Approx Float where
    distance x y = realToFrac $ x-y

instance Approx Double where
    distance x y = x-y

instance Approx Int where
    distance x y = fromIntegral (x-y)

instance Approx Integer where
    distance x y = fromIntegral (x-y)

instance Approx a => Approx (Maybe a) where
    distance Nothing Nothing = 0
    distance (Just _) Nothing = 10*tolerance
    distance Nothing (Just _) = 10*tolerance
    distance (Just x) (Just y) = distance x y

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module VecSpace
( VecSpace((+:), (*:), zero)
, EuclidVecSpace((.:))
) where

class Num b => VecSpace a b | a -> b where
    (+:) :: a -> a -> a
    (*:) :: Num b => b -> a -> a
    zero :: a

class VecSpace a b => EuclidVecSpace a b | a -> b where
    (.:) :: Num b => a -> a -> b
    mag2 :: a -> b
    mag2 v = v .: v
    mag :: Floating b => a -> b
    mag = sqrt . mag2

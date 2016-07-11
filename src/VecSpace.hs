{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module VecSpace
( VecSpace(..)
, EuclidVecSpace(..)
) where

class Num b => VecSpace b a | a -> b where
    (+:) :: a -> a -> a
    (-:) :: a -> a -> a
    x -: y = x +: ((-1) *: y)
    (*:) :: Num b => b -> a -> a
    zero :: a

class VecSpace b a => EuclidVecSpace b a | a -> b where
    (.:) :: Num b => a -> a -> b
    mag2 :: a -> b
    mag2 v = v .: v
    mag :: Floating b => a -> b
    mag = sqrt . mag2

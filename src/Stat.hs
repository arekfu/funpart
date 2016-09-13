module Stat
( empty
, score
, scoreW
, mean
, variance
, rms
, SVar
) where

data SVar a = SVar !a   -- ^ Sum of values.
                   !a   -- ^ Sum of squares of the values.
                   !a   -- ^ Total weight.
                   !Int -- ^ Total number of scores.

instance Show a => Show (SVar a) where
    show (SVar s s2 w n) = show (s, s2, w, n)

empty :: Num a => SVar a
empty = SVar 0 0 0 0

score :: Num a => SVar a -> a -> SVar a
score sv = scoreW sv 1

scoreW :: Num a => SVar a   -- ^ Initial value of the SVar.
                -> a        -- ^ Weight of the new value.
                -> a        -- ^ The new value.
                -> SVar a   -- ^ The updated SVar.
scoreW (SVar s s2 w n) w' x = SVar (s+w'*x) (s2 + (w'*x)^(2::Int)) (w+w') (n+1)

mean :: Fractional a => SVar a -> a
mean (SVar s _ _ n) = s/fromIntegral n

variance :: Fractional a => SVar a -> Maybe a
variance (SVar s s2  _ n) 
    | n>1 = Just $ (s2-s^(2::Int)/n')/(n'-1)
    | otherwise = Nothing
    where n' = fromIntegral n

rms :: Floating a => SVar a -> Maybe a
rms = fmap sqrt . variance

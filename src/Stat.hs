module Stat
( SVar
, empty
, tally
, tallyW
, mean
, variance
, rms
, sigmasFrom
) where

data SVar a = SVar !a   -- ^ Sum of values.
                   !a   -- ^ Sum of squares of the values.
                   !a   -- ^ Total weight.
                   !Int -- ^ Total number of scores.

instance Show a => Show (SVar a) where
    show (SVar s s2 w n) = show (s, s2, w, n)

instance Eq a => Eq (SVar a) where
    (SVar s s2 w n) == (SVar s' s2' w' n') =
        s==s' && s2==s2' && w==w' && n==n'


empty :: Num a => SVar a
empty = SVar 0 0 0 0

tally :: Num a => SVar a -> a -> SVar a
tally sv = tallyW sv 1

tallyW :: Num a => SVar a   -- ^ Initial value of the SVar.
                -> a        -- ^ Weight of the new value.
                -> a        -- ^ The new value.
                -> SVar a   -- ^ The updated SVar.
tallyW (SVar s s2 w n) w' x = SVar (s+w'*x) (s2 + (w'*x)^(2::Int)) (w+w') (n+1)

mean :: Fractional a => SVar a -> a
mean (SVar s _ _ n) = s/fromIntegral n

variance :: Fractional a => SVar a -> Maybe a
variance (SVar s s2  _ n) 
    | n>1 = Just $ (s2-s^(2::Int)/n')/((n'-1)*n')
    | otherwise = Nothing
    where n' = fromIntegral n

rms :: Floating a => SVar a -> Maybe a
rms = fmap sqrt . variance

sigmasFrom :: Floating a
           => SVar a    -- ^ An SVar to test.
           -> a         -- ^ The value to compare to.
           -> Maybe a   -- ^ The discrepancy, in units of standard deviations.
sigmasFrom sv ref = (\d -> (mean sv - ref) / d) <$> rms sv

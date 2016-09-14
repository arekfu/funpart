module Physics.FunPart.Stat
( SVar
, empty
, tally
, tallyW
, mean
, variance
, rms
, sigmasFrom
, display
) where

data SVar a = SVar !a !a !a !a  -- ^ Sum of values, sum of squares of the
                                -- values, sum of the weights and sum of the
                                -- squares of the weights.

instance Show a => Show (SVar a) where
    show (SVar s s2 w w2) = show (s, s2, w, w2)

instance Eq a => Eq (SVar a) where
    (SVar s s2 w w2) == (SVar s' s2' w' w2') =
        s==s' && s2==s2' && w==w' && w2==w2'


-- | Generate an empty SVar.
empty :: Num a => SVar a
empty = SVar 0 0 0 0

-- | Tally a value with unit weight.
tally :: Num a => SVar a -> a -> SVar a
tally sv x = tallyW sv x 1

-- | Tally a value with a given weight.
tallyW :: Num a => SVar a   -- ^ Initial value of the SVar.
                -> a        -- ^ The value to tally.
                -> a        -- ^ The weight of the value to tally.
                -> SVar a   -- ^ The updated SVar.
tallyW (SVar s s2 w w2) x wt = SVar (s+wt*x) (s2 + wt*x^(2::Int)) (w+wt) (w2+wt^(2::Int))

mean :: Fractional a => SVar a -> a
mean (SVar s _ w _) = s/w

variance :: (Ord a, Fractional a) => SVar a -> Maybe a
variance (SVar s s2 w w2) = if den <=0
                            then Nothing
                            else Just $ w2*(s2*w-s^(2::Int))/(den*w^(2::Int))
    where den = w^(2::Int)-w2

rms :: (Ord a, Floating a) => SVar a -> Maybe a
rms = fmap sqrt . variance

sigmasFrom :: (Ord a, Floating a)
           => SVar a    -- ^ An SVar to test.
           -> a         -- ^ The value to compare to.
           -> Maybe a   -- ^ The discrepancy, in units of standard deviations.
sigmasFrom sv ref = (\d -> (mean sv - ref) / d) <$> rms sv

display :: (Show a, Fractional a, Ord a, Floating a)
        => SVar a -> String
display sv = show (mean sv) ++ " +- " ++ uncertainty
    where uncertainty = case rms sv of
                            Nothing -> "n/a"
                            Just r -> show r

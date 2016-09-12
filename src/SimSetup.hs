module SimSetup
( SimSetup(..)
) where

import CrossSection
import Source
import Score
import MC (Seed)

data SimSetup =
    SimSetup { theXSec     :: CrossSection
             , initialSeed :: Seed
             , nShots      :: Int
             , source      :: Source
             , scores      :: [Score]
             }

instance Show SimSetup where
    show (SimSetup _ seed n _ _) = show seed ++ "; " ++ show n

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
             , nShots      :: Integer
             , source      :: Source
             , scores      :: [Score]
             }

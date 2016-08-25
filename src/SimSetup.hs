module SimSetup
( SimSetup(..)
) where

import CrossSection
import Source
import Score
import MC (Seed)

data SimSetup =
    SimSetup { theAbsXSec  :: CrossSectionValue
             , theTotXSec  :: CrossSectionValue
             , initialSeed :: Seed
             , nShots      :: Integer
             , source      :: Source
             , scores      :: [Score]
             }

module SimSetup
( SimSetup(..)
) where

import CrossSection
import MC (Seed)

data SimSetup = SimSetup { theAbsXSec  :: CrossSectionValue
                         , theTotXSec  :: CrossSectionValue
                         , initialSeed :: Seed
                         } deriving (Show, Eq, Ord)

module Physics.FunPart.SimSetup
( SimSetup(..)
) where

import Physics.FunPart.CrossSection
import Physics.FunPart.Source
import Physics.FunPart.Score
import Physics.FunPart.MC (Seed)

data SimSetup =
    SimSetup { theXSec     :: CrossSection
             , initialSeed :: Seed
             , nShots      :: Int
             , source      :: Source
             , scores      :: [Score]
             }

instance Show SimSetup where
    show (SimSetup _ seed n _ _) = show seed ++ "; " ++ show n

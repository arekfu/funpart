module Problem
( Problem
, runProblem
) where

import Control.Monad.Reader (runReaderT, ReaderT)
import System.Random (mkStdGen)

import SimSetup
import MC
import Score

type Problem a = ReaderT SimSetup MC [a]

runProblem :: Score a => Problem a -> SimSetup -> [a]
runProblem problem setup = fst $ runMC (runReaderT problem setup) initialGen
                            where seed = initialSeed setup
                                  initialGen = mkStdGen seed



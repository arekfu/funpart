module Problem
( Problem
, runProblem
) where

import Control.Monad.Reader (runReaderT, ReaderT)
import System.Random (mkStdGen)

import SimSetup
import MC

type Scores = Int

type Problem = ReaderT SimSetup MC Scores

runProblem :: Problem -> SimSetup -> Scores
runProblem problem setup = fst $ runMC (runReaderT problem setup) initialGen
                            where seed = initialSeed setup
                                  initialGen = mkStdGen seed

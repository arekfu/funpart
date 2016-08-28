module Problem.Common
( Problem
, runHistory
, runProblem
) where

import Control.Monad.Reader (runReaderT, ReaderT, ask, lift)
import System.Random (mkStdGen)

import MC
import qualified SimSetup
import Particle
import Track
import Source

type Problem a = ReaderT SimSetup.SimSetup MC a

runProblem :: Problem a -> SimSetup.SimSetup -> a
runProblem problem setup = fst $ runMC (runReaderT problem setup) initialGen
                            where seed = SimSetup.initialSeed setup
                                  initialGen = mkStdGen seed

-- solve one transport history
solve :: [Particle] -> Problem [Track]
solve = undefined

runHistory :: Integer -> Problem [Track]
runHistory _ = do
    (SimSetup.SimSetup _ _ _ _ source _) <- ask
    particles <- lift $ sampleParticles source
    solve particles

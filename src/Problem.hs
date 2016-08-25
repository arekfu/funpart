module Problem
( Problem
, runProblem
, solve
, runHistory
, fixedSourceProblem
) where

import Data.List (foldl')
import Control.Monad.Reader (runReaderT, ReaderT, ask, asks, lift, forM)
import System.Random (mkStdGen)

import qualified SimSetup
import MC
import Score
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


fixedSourceProblem :: Problem [Score]
fixedSourceProblem = do
    (SimSetup.SimSetup _ _ _ _ _ scores) <- ask
    nShots <- asks SimSetup.nShots
    tracks <- forM [1..nShots] runHistory
    return $ foldl' updateAllByTracks scores tracks

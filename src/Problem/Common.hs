{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Problem.Common
( Problem
, runHistory
, runProblem
) where

import Control.Monad.Reader (runReaderT, ReaderT, ask, lift, MonadReader)
import Control.Monad.State (MonadState)
import System.Random (mkStdGen, StdGen)

import MC
import qualified SimSetup
import Particle
import Track
import Source

newtype Problem a = Problem { unProblem :: ReaderT SimSetup.SimSetup MC a }
                    deriving (Monad, MonadReader SimSetup.SimSetup, MonadState StdGen, Applicative, Functor)

runProblem :: Problem a -> SimSetup.SimSetup -> a
runProblem problem setup = fst $ runMC (runReaderT problem' setup) initialGen
                            where seed = SimSetup.initialSeed setup
                                  initialGen = mkStdGen seed
                                  problem' = unProblem problem

-- solve one transport history
solve :: [Particle] -> Problem [Track]
solve = undefined

runHistory :: Integer -> Problem [Track]
runHistory _ = do
    (SimSetup.SimSetup _ _ _ _ source _) <- ask
    particles <- Problem $ lift $ sampleParticles source
    solve particles

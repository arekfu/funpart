{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Problem
( Problem
, runProblem
, runSimulation
, fixedSourceProblem
) where

import Control.Monad.RWS (RWS, evalRWS, asks, replicateM_, MonadReader, MonadState, MonadWriter)
import System.Random (mkStdGen, StdGen)

import qualified SimSetup
import Problem.Common
import Score
import Track

newtype Problem a = Problem { unProblem :: RWS SimSetup.SimSetup [Track] StdGen a }
                    deriving (Monad, MonadReader SimSetup.SimSetup, MonadState StdGen, MonadWriter [Track], Applicative, Functor)

runProblem :: Problem a -> SimSetup.SimSetup -> (a, [Track])
runProblem problem setup = result
    where seed = SimSetup.initialSeed setup
          initialGen = mkStdGen seed
          problem' = unProblem problem
          result = evalRWS problem' setup initialGen

runSimulation :: Problem a -> SimSetup.SimSetup -> [Score]
runSimulation problem setup = scores
                           where tracks = snd $ runProblem problem setup
                                 emptyScores = SimSetup.scores setup
                                 scores = updateAllByBatch emptyScores tracks

fixedSourceProblem :: (MonadReader SimSetup.SimSetup m, MonadState StdGen m, MonadWriter [Track] m)
                   => m ()
fixedSourceProblem = do
    nShots <- asks SimSetup.nShots
    replicateM_ nShots runHistory
    return ()

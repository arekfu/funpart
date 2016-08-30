{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Problem
( Problem
, runProblem
, fixedSourceProblem
) where

import Control.Monad.RWS (RWS, evalRWS, asks, forM_, MonadReader, MonadState, MonadWriter)
import System.Random (mkStdGen, StdGen)
import Data.Foldable (foldl')

import qualified SimSetup
import Problem.Common
import Score
import Track

newtype Problem a = Problem { unProblem :: RWS SimSetup.SimSetup [Track] StdGen a }
                    deriving (Monad, MonadReader SimSetup.SimSetup, MonadState StdGen, MonadWriter [Track], Applicative, Functor)

runProblem :: Problem () -> SimSetup.SimSetup -> [Score]
runProblem problem setup = scores
                           where seed = SimSetup.initialSeed setup
                                 initialGen = mkStdGen seed
                                 problem' = unProblem problem
                                 tracks = snd $ evalRWS problem' setup initialGen
                                 emptyScores = SimSetup.scores setup
                                 scores = foldl' updateAllByTrack emptyScores tracks

fixedSourceProblem :: (MonadReader SimSetup.SimSetup m, MonadState StdGen m, MonadWriter [Track] m)
                   => m ()
fixedSourceProblem = do
    nShots <- asks SimSetup.nShots
    forM_ [1..nShots] $ const runHistory
    return ()

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Problem
( Problem
, runProblem
, runAnyProblem
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

runAnyProblem :: Problem a -> SimSetup.SimSetup -> (a, [Track])
runAnyProblem problem setup = result
    where seed = SimSetup.initialSeed setup
          initialGen = mkStdGen seed
          problem' = unProblem problem
          result = evalRWS problem' setup initialGen

runProblem :: Problem () -> SimSetup.SimSetup -> [Score]
runProblem problem setup = scores
                           where tracks = snd $ runAnyProblem problem setup
                                 emptyScores = SimSetup.scores setup
                                 scores = foldl' updateAllByTrack emptyScores tracks

fixedSourceProblem :: (MonadReader SimSetup.SimSetup m, MonadState StdGen m, MonadWriter [Track] m)
                   => m ()
fixedSourceProblem = do
    nShots <- asks SimSetup.nShots
    forM_ [1..nShots] $ const runHistory
    return ()

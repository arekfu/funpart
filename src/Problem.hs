{-# LANGUAGE FlexibleContexts #-}

module Problem
( Problem
, runProblem
, fixedSourceProblem
) where

import Control.Monad.RWS (asks, forM_, MonadReader, MonadState, MonadWriter)
import System.Random (StdGen)

import qualified SimSetup
import Problem.Common
import Track

fixedSourceProblem :: (MonadReader SimSetup.SimSetup m, MonadState StdGen m, MonadWriter [Track] m)
                   => m ()
fixedSourceProblem = do
    nShots <- asks SimSetup.nShots
    forM_ [1..nShots] $ const runHistory
    return ()

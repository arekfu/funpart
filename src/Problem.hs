module Problem
( Problem
, runProblem
, fixedSourceProblem
) where

import Control.Monad.Reader (ask, asks, forM)
import Data.List (foldl')

import qualified SimSetup
import Score
import Problem.Common

fixedSourceProblem :: Problem [Score]
fixedSourceProblem = do
    (SimSetup.SimSetup _ _ _ _ _ scores) <- ask
    nShots <- asks SimSetup.nShots
    tracks <- forM [1..nShots] runHistory
    return $ foldl' updateAllByTracks scores tracks

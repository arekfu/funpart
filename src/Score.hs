{-# LANGUAGE TemplateHaskell, GADTs #-}

module Score
( ScoreLike(..)
, ScoreValue
, CollFlux(..)
, Score(..)
, collFluxValue
, initCollFluxScore
, updateAllByTrackPoint
, updateAllByTrack
, updateAllByTracks
) where

import Control.Lens
import Data.List (foldl')

import Core
import Track

type ScoreValue = FPFloat

class ScoreLike a where
    updateByTrackPoint :: a -> TrackPoint -> a
    updateByTrack :: a -> Track -> a
    updateByTrack score track = foldl' updateByTrackPoint score (_trackPoints track)
    updateByTracks :: a -> [Track] -> a
    updateByTracks = foldl' updateByTrack
    display :: a -> IO ()

updateAllByTrackPoint :: ScoreLike a => [a] -> TrackPoint -> [a]
updateAllByTrackPoint scores trackPoint = map (`updateByTrackPoint` trackPoint) scores

updateAllByTrack :: ScoreLike a => [a] -> Track -> [a]
updateAllByTrack scores track = map (`updateByTrack` track) scores

updateAllByTracks :: ScoreLike a => [a] -> [Track] -> [a]
updateAllByTracks scores tracks = map (`updateByTracks` tracks) scores


-- now introduce a wrapper GADT for abstract collections of ScoreLike objects
data Score where
    Score :: ScoreLike a => a -> Score

instance ScoreLike Score where
    updateByTrackPoint (Score s) p = Score $ updateByTrackPoint s p
    display (Score s) = display s

newtype CollFlux = CollFlux { _collFluxValue :: ScoreValue }
    deriving (Show, Eq, Ord)

-- make all the necessary lenses
makeLenses ''CollFlux

initCollFluxScore :: CollFlux
initCollFluxScore = CollFlux 0.0

instance ScoreLike CollFlux where
    updateByTrackPoint score trackPoint =
        case trackPoint^.pointType of
            CollisionPoint xs _ -> over collFluxValue (+(1.0/xs)) score
            _ -> score
    display score = putStrLn ("collision flux: " ++ show (score^.collFluxValue))

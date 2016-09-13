{-# LANGUAGE TemplateHaskell, GADTs #-}

module Score (
-- * Classes
  ScoreLike(..)
-- * Types
, Score(..)
, CollFlux(..)
, ScoreValue
-- * Functions
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
import Stat

-- | A type synonim for more expressive function signatures.
type ScoreValue = SVar FPFloat

{-| The class of types that behave like scores. Types in this class must
    define:

       1. how to update the score values from particle tracks
          ('updateByTrackPoint' at least);

       2. how to convert the score value to a character string ('display').
-}
class ScoreLike a where
    -- | Update the score with the track point.
    updateByTrackPoint :: a -> TrackPoint -> a

    -- | /Default implementation/: a fold over all the track points.
    updateByTrack :: a -> Track -> a
    updateByTrack score track = foldl' updateByTrackPoint score (_trackPoints track)

    -- | /Default implementation/: a fold over all the tracks.
    updateByTracks :: a -> [Track] -> a
    updateByTracks = foldl' updateByTrack

    -- | Convert the score to a character string.
    display :: a -> String


-- | Helper function to map updateByTrackPoint on a list of scores.
updateAllByTrackPoint :: ScoreLike a => [a] -> TrackPoint -> [a]
updateAllByTrackPoint scores trackPoint = map (`updateByTrackPoint` trackPoint) scores

-- | Helper function to map updateByTrack on a list of scores.
updateAllByTrack :: ScoreLike a => [a] -> Track -> [a]
updateAllByTrack scores track = map (`updateByTrack` track) scores

-- | Helper function to map updateByTracks on a list of scores.
updateAllByTracks :: ScoreLike a => [a] -> [Track] -> [a]
updateAllByTracks scores tracks = map (`updateByTracks` tracks) scores


-- | A wrapper GADT for heterogeneous collections of ScoreLike objects.
data Score where
    Score :: ScoreLike a => a -> Score

-- | Trivial ScoreLike instance for Score objects.
instance ScoreLike Score where
    updateByTrackPoint (Score s) p = Score $ updateByTrackPoint s p
    display (Score s) = display s

-- | A collision-based flux score
newtype CollFlux = CollFlux { _collFluxValue :: ScoreValue }
    deriving (Show, Eq)

-- make all the necessary lenses
makeLenses ''CollFlux

-- | The collision-based flux score is incremented by /w\/&#x3A3;/, where /w/ is the particle weight and /&#x3A3; / is the total macroscopic cross section.
instance ScoreLike CollFlux where
    updateByTrackPoint score trackPoint =
        case trackPoint^.pointType of
            CollisionPoint xs _ -> over collFluxValue (\v -> tally v (1.0/xs)) score
            _ -> score
    display score = "collision flux: " ++ show (score^.collFluxValue)

-- | Generate an empty collision flux.
initCollFluxScore :: CollFlux
initCollFluxScore = CollFlux empty

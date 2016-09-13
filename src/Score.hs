module Score (
-- * Types
  Score(..)
, ScoreValue
-- * Functions
, updateAllByTracks
) where

import Control.Lens
import Data.List (foldl')

import Core
import Track
import Stat

-- | A type synonim for more expressive function signatures.
type ScoreValue = SVar FPFloat

updateByTrackPoint :: Score -> TrackPoint -> Score
updateByTrackPoint score@(CollFlux val) trackPoint =
        case trackPoint^.pointType of
            CollisionPoint xs _ -> CollFlux $ tally val (1.0/xs)
            _ -> score
updateByTrackPoint _ _ = undefined

updateByTrack :: Score -> Track -> Score
updateByTrack (TrackLength val) track =  TrackLength $ tally val len
        where len = fromIntegral . length $ track^.trackPoints
-- | /Default implementation/: a fold over all the track points.
updateByTrack score track = foldl' updateByTrackPoint score (_trackPoints track)

-- | /Default implementation/: a fold over all the tracks.
updateByTracks :: Score -> [Track] -> Score
updateByTracks = foldl' updateByTrack

-- | Helper function to map updateByTracks on a list of scores.
updateAllByTracks :: [Score] -> [Track] -> [Score]
updateAllByTracks scores tracks = map (`updateByTracks` tracks) scores


-- | A wrapper GADT for heterogeneous collections of ScoreLike objects.
data Score = CollFlux ScoreValue       -- ^ A collision-based flux score.
           | TrackLength ScoreValue    -- ^ A track-length score.
    deriving (Show, Eq)

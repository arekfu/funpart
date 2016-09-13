module Score (
-- * Types
  Score(..)
, ScoreValue
-- * Functions
, updateAllByBatch
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
updateByBatch :: Score -> [Track] -> Score
updateByBatch = foldl' updateByTrack

-- | Helper function to map updateByBatch on a list of scores.
updateAllByBatch :: [Score] -> [Track] -> [Score]
updateAllByBatch scores tracks = map (`updateByBatch` tracks) scores


-- | A union type for different scores.
data Score = CollFlux ScoreValue       -- ^ A collision-based flux score.
           | TrackLength ScoreValue    -- ^ A track-length score.
    deriving (Show, Eq)

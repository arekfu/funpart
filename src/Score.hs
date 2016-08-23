{-# LANGUAGE TemplateHaskell #-}

module Score
( Score(..)
, ScoreValue
, CollFlux(..)
, collFluxValue
) where

import Control.Lens
import Data.List (foldl')

import Core
import Track

type ScoreValue = FPFloat

class Score a where
    updateTrackPoint :: a -> TrackPoint -> a
    updateTrack :: a -> Track -> a
    updateTrack score track = foldl' updateTrackPoint score (_trackPoints track)
    updateTracks :: a -> [Track] -> a
    updateTracks = foldl' updateTrack
    display :: a -> IO ()

newtype CollFlux = CollFlux { _collFluxValue :: ScoreValue }
    deriving (Show, Eq, Ord)
makeLenses ''CollFlux

instance Score CollFlux where
    updateTrackPoint score trackPoint =
        case trackPoint^.pointType of
            CollisionPoint xs _ -> over collFluxValue (+(1.0/xs)) score
            _ -> score
    display score = putStrLn ("collision flux: " ++ show (score^.collFluxValue))

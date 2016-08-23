{-# LANGUAGE TemplateHaskell #-}

module CrossSection
( CrossSectionValue
, CrossSection(..)
, totXSConst
, absXSConst
) where

import Control.Lens (makeLenses)

import Core
import Particle

type CrossSectionValue = FPFloat

class CrossSection a where
    getAbsXS :: Dynamic p => a -> p -> CrossSectionValue
    getAbsXS xs p = max 0.0 $ getTotXS xs p - getScatXS xs p
    getTotXS :: Dynamic p => a -> p -> CrossSectionValue
    getTotXS xs p = max 0.0 $ getAbsXS xs p + getScatXS xs p
    getScatXS :: Dynamic p => a -> p -> CrossSectionValue
    getScatXS xs p = max 0.0 $ getTotXS xs p - getAbsXS xs p

data ConstantXS = ConstantXS { _totXSConst :: CrossSectionValue
                             , _absXSConst :: CrossSectionValue
                             } deriving (Show, Eq)
makeLenses ''ConstantXS

instance CrossSection ConstantXS where
    getAbsXS (ConstantXS _ absXS) _ = absXS
    getTotXS (ConstantXS totXS _) _ = totXS

{-# LANGUAGE TemplateHaskell, GADTs #-}

module Physics.FunPart.CrossSection
( CrossSectionValue
, CrossSectionLike(..)
, CrossSection(..)
, totXSConst
, absXSConst
, ConstantXS(..)
) where

import Control.Lens (makeLenses)

import Physics.FunPart.Core
import Physics.FunPart.Particle

type CrossSectionValue = FPFloat

class CrossSectionLike a where
    getAbsXS :: Dynamic p => a -> p -> CrossSectionValue
    getAbsXS xs p = max 0.0 $ getTotXS xs p - getScatXS xs p
    getTotXS :: Dynamic p => a -> p -> CrossSectionValue
    getTotXS xs p = max 0.0 $ getAbsXS xs p + getScatXS xs p
    getScatXS :: Dynamic p => a -> p -> CrossSectionValue
    getScatXS xs p = max 0.0 $ getTotXS xs p - getAbsXS xs p

data CrossSection where
    CrossSection :: CrossSectionLike a => a -> CrossSection

instance CrossSectionLike CrossSection where
    getAbsXS (CrossSection xs) = getAbsXS xs
    getTotXS (CrossSection xs) = getTotXS xs
    getScatXS (CrossSection xs) = getScatXS xs


data ConstantXS = ConstantXS { _totXSConst :: CrossSectionValue
                             , _absXSConst :: CrossSectionValue
                             } deriving (Show, Eq)
makeLenses ''ConstantXS

instance CrossSectionLike ConstantXS where
    getAbsXS (ConstantXS _ absXS) _ = absXS
    getTotXS (ConstantXS totXS _) _ = totXS

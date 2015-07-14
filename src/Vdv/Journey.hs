{-# LANGUAGE TemplateHaskell #-}
module Vdv.Journey where

import ClassyPrelude hiding(Element)
import Control.Lens(makeLenses)
import Text.XML(Element)

data Journey = Journey {
    _journeyElement :: Element
  , _journeyZst :: Text
  } deriving(Show,Eq)

$(makeLenses ''Journey)

{-# LANGUAGE TemplateHaskell #-}
module Vdv.Filter where

import ClassyPrelude
import Control.Lens(makeLenses)
import Vdv.FilterOperator

data Filter = Filter {
    _filterTagName :: Text
  , _filterOperator :: FilterOperator
  , _filterTagValue :: Text
  } deriving(Show,Eq)

$(makeLenses ''Filter)

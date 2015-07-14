{-# LANGUAGE TemplateHaskell #-}
module Vdv.FilterOperator where

import ClassyPrelude
import Control.Lens(makePrisms)

data FilterOperator = FilterOperatorEq
                    | FilterOperatorLike
                    deriving(Show,Eq)

$(makePrisms ''FilterOperator)

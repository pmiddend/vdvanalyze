{-# LANGUAGE TemplateHaskell #-}
module Vdv.FilterOperator where

import ClassyPrelude
import Control.Lens(makePrisms)

data FilterOperator = FilterOperatorEq
                    | FilterOperatorLike
                    | FilterOperatorLe
                    | FilterOperatorGe
                    | FilterOperatorDateLe
                    | FilterOperatorDateGe
                    | FilterOperatorDateEq
                    deriving(Show,Eq)

$(makePrisms ''FilterOperator)

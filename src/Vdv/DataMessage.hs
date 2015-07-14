module Vdv.DataMessage where

import ClassyPrelude
import Control.Lens(Iso',iso)
import Text.XML(Document)

newtype DataMessage = DataMessage Document deriving(Show)

dataMessageDoc :: Iso' DataMessage Document
dataMessageDoc = iso (\(DataMessage d) -> d) DataMessage

module Vdv.XML where

import ClassyPrelude hiding (Element)
import Text.XML

elementToBareDocument :: Element -> Document
elementToBareDocument e =
  Document {
      documentPrologue = Prologue { prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}
    , documentRoot = e
    , documentEpilogue = []
  }

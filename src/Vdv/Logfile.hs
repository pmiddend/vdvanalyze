module Vdv.Logfile where

import ClassyPrelude
import Control.Lens(iso,Iso')

newtype Logfile = Logfile Text

logfileAsText :: Iso' Logfile Text
logfileAsText = iso (\(Logfile t) -> t) Logfile

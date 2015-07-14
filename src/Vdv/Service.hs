{-# LANGUAGE TemplateHaskell #-}
module Vdv.Service where

import ClassyPrelude
import Control.Lens(makePrisms)

data Service = ServiceAUS
             | ServiceDFI
             deriving(Eq,Show,Bounded,Read,Enum)

$(makePrisms ''Service)

serviceContainer :: Service -> Text
serviceContainer ServiceAUS = "AUSNachricht"
serviceContainer ServiceDFI = "AZBNachricht"

journeyContainer :: Service -> Text
journeyContainer ServiceAUS = "IstFahrt"
journeyContainer ServiceDFI = "AZBFahrplanLage"

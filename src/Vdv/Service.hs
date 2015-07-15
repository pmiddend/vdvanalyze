{-# LANGUAGE TemplateHaskell #-}
module Vdv.Service where

import ClassyPrelude
import Control.Lens(makePrisms)
import Options.Applicative(ReadM,eitherReader)

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

parseService :: String -> Either String Service
parseService "AUS" = Right ServiceAUS
parseService "DFI" = Right ServiceDFI
parseService s = Left ("Invalid service " <> s)

serviceOpt :: ReadM Service
serviceOpt = eitherReader parseService

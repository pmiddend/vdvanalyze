{-# LANGUAGE RankNTypes #-}
module Main where

import Text.XML.Lens(attr,el,root,entire,text,ell,(./))
import ClassyPrelude hiding (Element,FilePath)
import Control.Lens(iso,(^.),from,(^?!),(^..),Traversal',view,to,has,filtered)
import System.FilePath
import Data.List(unfoldr)
import qualified Data.ByteString as BIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Text.XML(parseText_,renderText,RenderSettings(..))
import Data.Default(def)
import Vdv.Logfile
import Vdv.DataMessage
import Vdv.FilterOperator
import Vdv.XML
import Vdv.Text
import Vdv.Settings
import Vdv.Filter
import Vdv.Service
import Vdv.Journey

-- Some global constants
daaTag :: Text
daaTag = "DatenAbrufenAntwort"
daaTagStart :: Text
daaTagStart = "<" <> daaTag
daaTagEnd :: Text
daaTagEnd = "</" <> daaTag <> ">"

-- Some data types with corresponding lenses
readLogfile :: (MonadIO m,Functor m) => FilePath -> m Logfile
readLogfile fp = do
  contents <- liftIO (BIO.readFile fp)
  return ((TE.decodeLatin1 contents) ^. from logfileAsText)

extractData :: Text -> Maybe (Text, Text)
extractData = extractDelimited daaTagStart daaTagEnd

extractDataMessages :: Logfile -> [DataMessage]
extractDataMessages t =
  (DataMessage . parseText_ def . TL.fromStrict) <$> (unfoldr extractData (t ^. logfileAsText))
   
extractJourneys :: Service -> DataMessage -> [Journey]
extractJourneys service dm =
  let
    zst = dm ^?! extractZst
  in
    dm ^.. extractJourneysLens service zst

extractJourneysLens :: Service -> Text -> Traversal' DataMessage Journey
extractJourneysLens service zst = dataMessageDoc . root . ell daaTag ./ ell (serviceContainer service) ./ ell (journeyContainer service) . (iso (\j -> Journey{_journeyElement=j,_journeyZst=zst}) (view journeyElement))

extractZst :: Traversal' DataMessage Text
extractZst = dataMessageDoc . root . entire . el "Bestaetigung" . attr "Zst"

outputJourneys :: MonadIO m => [Journey] -> m ()
outputJourneys = mapM_ print

filterJourney :: [Filter] -> Journey -> Bool
filterJourney fs j = and (filterJourneySingle j <$> fs)

filterJourneySingle :: Journey -> Filter -> Bool
filterJourneySingle j f = has (journeyElement . entire . ell (f ^. filterTagName) . text . filtered (evalFilterOp (f ^. filterOperator) (f ^. filterTagValue))) j
  where evalFilterOp FilterOperatorEq expected actual = expected == actual
        evalFilterOp FilterOperatorLike expected actual = expected `isInfixOf` actual
        --evalFilterOp _ _ _ = error "Invalid filter"

prettyPrintJourney :: Journey -> Text
prettyPrintJourney j =
  (j ^. journeyZst) <> "\n" <> TL.toStrict (renderText (def { rsPretty = True } ) (j ^. journeyElement . to elementToBareDocument))

main :: IO ()
main = do
  settings <- parseSettings
  logfile <- readLogfile (settings ^. settingsInputFile)
  let filteredJourneys = filter (filterJourney (settings ^. settingsFilters)) (extractDataMessages logfile >>= (extractJourneys (settings ^. settingsService)))
  mapM_ (putStrLn . prettyPrintJourney) ((if settings ^. settingsInvert then reverse else id) filteredJourneys)

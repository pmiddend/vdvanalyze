{-# LANGUAGE RankNTypes #-}
module Main where

import Text.XML.Lens(attr,el,root,entire,text,ell,(./),localName,nodes,_Element,Element,nameLocalName,elementName)
import ClassyPrelude hiding (Element,FilePath)
import Control.Lens(only,iso,(^.),from,(^?!),(^..),Traversal',view,to,has,filtered,(%~),(&),_head,plate)
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
import Vdv.Time
import Vdv.ElementPath
import Vdv.Exclusions
import Vdv.Text
import Vdv.Settings
import Vdv.Filter
import Vdv.Service
import Vdv.Journey
import qualified Data.CaseInsensitive as CI

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
  return (TE.decodeLatin1 contents ^. from logfileAsText)

extractData :: Text -> Maybe (Text, Text)
extractData = extractDelimited daaTagStart daaTagEnd

extractDataMessages :: Logfile -> [DataMessage]
extractDataMessages t =
  (DataMessage . parseText_ def . TL.fromStrict) <$> unfoldr extractData (t ^. logfileAsText)
   
extractJourneys :: Service -> DataMessage -> [Journey]
extractJourneys service dm =
  let
    zst = dm ^?! extractZst
  in
    dm ^.. extractJourneysLens service zst

namedOf :: [CI.CI Text] -> Traversal' Element Element
namedOf ns f s
    | CI.mk (nameLocalName (elementName s)) `elem` ns = f s
    | otherwise = pure s

--extractJourneysLens :: Service -> Text -> Traversal' DataMessage Journey
extractJourneysLens :: Service -> Text -> Traversal' DataMessage Journey
extractJourneysLens service zst = dataMessageDoc . root . ell daaTag ./ ell (serviceContainer service) ./ namedOf (CI.mk <$> journeyContainer service) . iso (\j -> Journey{_journeyElement=j,_journeyZst=zst}) (view journeyElement)

extractZst :: Traversal' DataMessage Text
extractZst = dataMessageDoc . root . entire . el "Bestaetigung" . attr "Zst"

outputJourneys :: MonadIO m => [Journey] -> m ()
outputJourneys = mapM_ print

filterJourney :: [Filter] -> Journey -> Bool
filterJourney fs j = and (filterJourneySingle j <$> fs)

dateTextLe :: Text -> Text -> Bool
dateTextLe = isoDateOp (<=)

dateTextGe :: Text -> Text -> Bool
dateTextGe = isoDateOp (>=)

dateTextEq :: Text -> Text -> Bool
dateTextEq = isoDateOp (>=)

filterJourneySingle :: Journey -> Filter -> Bool
filterJourneySingle j f =
  if has (filterTagPath . pathElements . _head . only "$Zst") f
    then evalFilterOp (f ^. filterOperator) (f ^. filterTagValue) (j ^. journeyZst)
    else has (journeyElement . elementPathTraversal (f ^. filterTagPath) . text . filtered (evalFilterOp (f ^. filterOperator) (f ^. filterTagValue))) j
  where evalFilterOp FilterOperatorEq expected actual = expected == actual
        evalFilterOp FilterOperatorLike expected actual = expected `isInfixOf` actual
        evalFilterOp FilterOperatorGe expected actual = readUnsafeInt expected <= readUnsafeInt actual
        evalFilterOp FilterOperatorLe expected actual = readUnsafeInt expected >= readUnsafeInt actual
        evalFilterOp FilterOperatorDateLe expected actual = actual `dateTextLe` expected
        evalFilterOp FilterOperatorDateGe expected actual = actual `dateTextGe` expected
        evalFilterOp FilterOperatorDateEq expected actual = actual `dateTextEq` expected
        --evalFilterOp _ _ _ = error "Invalid filter"

prettyPrintJourney :: Journey -> Text
prettyPrintJourney j =
  (j ^. journeyZst) <> "\n" <> TL.toStrict (renderText (def { rsPretty = True } ) (j ^. journeyElement . to elementToBareDocument))

elementPathTraversal :: ElementPath -> Traversal' Element Element
elementPathTraversal = foldrElementPath (\e p -> plate . ell e . p) id

runExclusions :: Exclusions -> Journey -> Journey
runExclusions exclusions journey = foldrExclusion runExclusion journey exclusions
  where
    runExclusion :: ElementPath -> Journey -> Journey
    runExclusion exclusionPath previousJourney = previousJourney & journeyElement . (elementPathTraversal . pathInitUnsafe $ exclusionPath) . nodes %~ filterNodes (pathLastUnsafe exclusionPath)
    filterNodes name = filter (filterByName name)
    filterByName name = not . has (_Element . localName . only name)

-- Debug function
readFirstJourney :: (MonadIO m,Functor m) => FilePath -> m Element 
readFirstJourney fp = do
  f <- readLogfile fp
  let
    journeys :: [Journey]
    journeys = extractDataMessages f >>= extractJourneys ServiceAUS
  return (journeys ^?! _head . journeyElement)
  
main :: IO ()
main = do
  settings <- parseSettings
  logfile <- readLogfile (settings ^. settingsInputFile)
  let filteredJourneys = runExclusions (settings ^. settingsExclusions) <$> filter (filterJourney (settings ^. settingsFilters)) (extractDataMessages logfile >>= extractJourneys (settings ^. settingsService))
  mapM_ (putStrLn . prettyPrintJourney) ((if settings ^. settingsInvert then reverse else id) filteredJourneys)

module Main where

import Text.XML.Lens(Element)
import ClassyPrelude hiding (Element)
import Control.Lens(Iso',iso)

newtype Logfile = Logfile Text

logfileAsText :: Iso' Logfile Text
logfileAsText = iso (\(Logfile t) -> t) Logfile

newtype DataMessage = DataMessage Element

newtype TagName = TagName Text

data Journey = Journey {
    _journeyElement :: Element
  , _journeyZst :: Text
  } deriving(Show,Eq)

prettyPrintJourney :: Journey -> Text
prettyPrintJourney = error "prettyPrintJourney not implemented"

readLogfile :: (MonadIO m,Functor m) => FilePath -> m Logfile
readLogfile fp = Logfile <$> readFile fp

extractTags :: Text -> TagName -> [Text]
extractTags = error "extractTags not implemented"

breakOnSafe :: Text -> Maybe (Text,Text)
breakOnSafe = undefined

extractDataMessages :: Logfile -> [DataMessage]
extractDataMessages t = unfold f t
  where
    f t' = do
      (_,rest) <- breakOnSafe "<DatenAbrufenAnt" t'
      (daae,rest') <- breakOnSafe "</DatenAbrufenAnt" rest
      return ()
    

extractJourneys :: DataMessage -> [Journey]
extractJourneys = error "extractJourneys not implemented"

extractZst :: DataMessage -> Text
extractZst = error "extractZst not implemented"

outputDataMessages :: MonadIO m => [DataMessage] -> m ()
outputDataMessages = error "outputDataMessages not implemented"

filterJourney :: Filter -> Journey -> Bool
filterJourney = error "filterJourney not implemented"

data Filter = Filter {
    _filterTagName :: Text
  , _filterTagValue :: Text
  } deriving(Show,Eq)

parseFilters :: Text -> [Filter]
parseFilters = error "parseFilters not implemented"

data Settings = Settings {
    _settingsInputFile :: FilePath
  , _settingsFilters :: [Filter]
  , _settingsInvert :: Bool
  } deriving(Show,Eq)

main :: IO ()
main = do
  logfile <- readLogfile "data/test.xml"
  let
    dataMessages = extractDataMessages logfile
    journeys = concatMap extractJourneys dataMessages
  mapM_ (putStrLn . prettyPrintJourney) journeys
  return ()

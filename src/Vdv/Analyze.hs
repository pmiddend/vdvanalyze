module Main where

import Text.XML.Lens(Element)
import ClassyPrelude

newtype Logfile = Logfile Text

newtype DataMessage = DataMessage Element

newtype TagName = TagName Text

newtype Journey = Journey Element

prettyPrintDataMessage :: DataMessage -> Text
prettyPrintDataMessage = error "prettyPrintDataMessage not implemented"

readLogfile :: MonadIO m => FilePath -> m Logfile
readLogfile = error "readLogfile not implemented"

extractTags :: Text -> TagName -> [Text]
extractTags = error "extractTags not implemented"

extractDataMessages :: Logfile -> [DataMessage]
extractDataMessages = error "extractDataMessages not implemented"

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
  }

parseFilters :: Text -> [Filter]
parseFilters = error "parseFilters not implemented"

data Settings = Settings {
    _settingsInputFile :: FilePath
  , _settingsFilters :: [Filter]
  , _settingsInvert :: Bool
  } deriving(Show,Eq)

main :: IO ()
main = do
  logFile <- readLogFile "data/test.xml
  return ()

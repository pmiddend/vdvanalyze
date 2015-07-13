{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Text.XML.Lens(Element,Document,attr,el,root,entire)
import ClassyPrelude hiding (Element,FilePath)
import Control.Lens(Iso',iso,(^.),from,(^?!),(^..),Traversal',makeLenses,view,to)
import System.FilePath
import Data.Text(breakOn)
import Data.List(unfoldr)
import qualified Data.ByteString as BIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Text.XML(parseText_,renderText,RenderSettings(..),Document(..),Prologue(..))
import Data.Default(def)

newtype Logfile = Logfile Text

logfileAsText :: Iso' Logfile Text
logfileAsText = iso (\(Logfile t) -> t) Logfile

newtype DataMessage = DataMessage Document deriving(Show)

dataMessageDoc :: Iso' DataMessage Document
dataMessageDoc = iso (\(DataMessage d) -> d) DataMessage

newtype TagName = TagName Text

data Journey = Journey {
    _journeyElement :: Element
  , _journeyZst :: Text
  } deriving(Show,Eq)

$(makeLenses ''Journey)

readLogfile :: (MonadIO m,Functor m) => FilePath -> m Logfile
readLogfile fp = do
  contents <- liftIO (BIO.readFile fp)
  return ((TE.decodeLatin1 contents) ^. from logfileAsText)

extractTags :: Text -> TagName -> [Text]
extractTags = error "extractTags not implemented"

data BreakMode = BreakWithDelimiter
               | BreakWithoutDelimiter

breakOnSafe :: BreakMode -> Text -> Text -> Maybe (Text,Text)
breakOnSafe bm del t =
  case bm of
    BreakWithoutDelimiter ->
      case breakOn del t of
        (_,"") -> Nothing
        (x,y) -> return (x,y)
    BreakWithDelimiter ->
      case breakOn del t of
        (_,"") -> Nothing
        (x,y) -> return (x ++ del,drop (length del) y)

extractDelimited :: Text -> Text -> Text -> Maybe (Text,Text)
extractDelimited begin end t = do
  (_,rest) <- breakOnSafe BreakWithoutDelimiter begin t
  (daae,rest') <- breakOnSafe BreakWithDelimiter end rest
  return (daae,rest')

extractData :: Text -> Maybe (Text, Text)
extractData = extractDelimited "<DatenAbrufenAnt" "</DatenAbrufenAntwort>"

extractDataMessages :: Logfile -> [DataMessage]
extractDataMessages t =
  (DataMessage . parseText_ def . TL.fromStrict) <$> (unfoldr extractData (t ^. logfileAsText))
   
extractJourneys :: DataMessage -> [Journey]
extractJourneys dm =
  let
    zst = dm ^?! extractZst
  in
    dm ^.. extractJourneysLens zst

extractJourneysLens :: Text -> Traversal' DataMessage Journey
extractJourneysLens zst = dataMessageDoc . root . entire . el "IstFahrt" . (iso (\j -> Journey{_journeyElement=j,_journeyZst=zst}) (view journeyElement))

extractZst :: Traversal' DataMessage Text
extractZst = dataMessageDoc . root . entire . el "Bestaetigung" . attr "Zst"

outputJourneys :: MonadIO m => [Journey] -> m ()
outputJourneys = mapM_ print

filterJourney :: Filter -> Journey -> Bool
filterJourney _ _ = True

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

elementToDocument :: Element -> Document
elementToDocument e =
  Document {
      documentPrologue = Prologue { prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}
    , documentRoot = e
    , documentEpilogue = []
  }

prettyPrintJourney :: Journey -> Text
prettyPrintJourney j =
  (j ^. journeyZst) <> "\n" <> TL.toStrict (renderText (def { rsPretty = True} ) (j ^. journeyElement . to elementToDocument))

main :: IO ()
main = do
  logfile <- readLogfile "data/test.xml"
  let
    filter = undefined
    filteredJourneys :: [Journey]
    filteredJourneys = filter (filterJourney filter) (extractDataMessages logfile >>= extractJourneys)
  mapM_ (putStrLn . prettyPrintJourney) filteredJourneys

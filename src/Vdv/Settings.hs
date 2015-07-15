{-# LANGUAGE TemplateHaskell #-}
module Vdv.Settings where

import ClassyPrelude hiding(FilePath,(<>))
import System.FilePath
import Options.Applicative(strOption,long,help,option,(<>),Parser,execParser,helper,fullDesc,progDesc,header,info,switch,eitherReader,ReadM)
import Control.Lens(makeLenses)
import Vdv.Filter
import Vdv.Exclusions
import Vdv.FilterOperator
import Vdv.Service
import qualified Data.Attoparsec.Text as AP

data Settings = Settings {
    _settingsInputFile :: FilePath
  , _settingsFilters :: [Filter]
  , _settingsService :: Service
  , _settingsInvert :: Bool
  , _settingsExclusions :: Exclusions
  } deriving(Show,Eq)

$(makeLenses ''Settings)

filterParser :: AP.Parser [Filter]
filterParser = singleFilter  `AP.sepBy` (AP.char ',')
  where parseOperator '=' = FilterOperatorEq
        parseOperator '~' = FilterOperatorLike
        parseOperator o = error $ "Invalid operator " <> show o
        singleFilter = Filter <$> (pack <$> (AP.many1 (AP.satisfy (AP.notInClass "=~")))) <*> (parseOperator <$> AP.anyChar) <*> (pack <$> (AP.many1 (AP.notChar ',')))

parseFilters :: Text -> Either String [Filter]
parseFilters t = AP.parseOnly filterParser t

parseService :: String -> Either String Service
parseService "AUS" = Right ServiceAUS
parseService "DFI" = Right ServiceDFI
parseService s = Left ("Invalid service " <> s)

filtersOpt :: ReadM [Filter]
filtersOpt = eitherReader (parseFilters . pack)

serviceOpt :: ReadM Service
serviceOpt = eitherReader parseService

parseSettings' :: Parser Settings
parseSettings' = Settings
                <$> strOption (long "input-file" <> help "Logdatei zum Einlesen")
                <*> (option filtersOpt (long "filters" <> help "Menge von Filtern, Format <tag>[=~]<wert>") <|> pure mempty)
                <*> option serviceOpt (long "service" <> help "Dienst (AUS/DFI/...)")
                <*> switch (long "invert" <> help "Outputfahrten umdrehen")
                <*> (option exclusionsOpt (long "exclusions" <> help "Welche Tags bei der Ausgabe weggetan werden sollen (kommaseparierte Liste von Pfaden, mit / getrennt)") <|> pure mempty)

parseSettings :: MonadIO m => m Settings
parseSettings = liftIO (execParser opts)
  where
    opts = info (helper <*> parseSettings')
      (fullDesc <> progDesc "Parsen, filter und analysieren von VDV-Logdateien" <> header "Parsen, filter und analysieren von VDV-Logdateien")

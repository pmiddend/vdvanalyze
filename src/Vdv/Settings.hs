{-# LANGUAGE TemplateHaskell #-}
module Vdv.Settings where

import ClassyPrelude hiding(FilePath,(<>))
import System.FilePath
import Options.Applicative(strOption,long,help,option,(<>),Parser,execParser,helper,fullDesc,progDesc,header,info,switch)
import Control.Lens(makeLenses)
import Vdv.Filter
import Vdv.Exclusions
import Vdv.Service

data Settings = Settings {
    _settingsInputFile :: FilePath
  , _settingsFilters :: [Filter]
  , _settingsService :: Service
  , _settingsInvert :: Bool
  , _settingsExclusions :: Exclusions
  } deriving(Show,Eq)

$(makeLenses ''Settings)

parseSettings' :: Parser Settings
parseSettings' = Settings
                <$> strOption (long "input-file" <> help "Logdatei zum Einlesen")
                <*> (option filtersOpt (long "filters" <> help "Kommaserparierte Liste von Filtern, Format <pfad>[=~]<wert> ist hierbei ein Pfad tagname1/tagname2/...") <|> pure mempty)
                <*> option serviceOpt (long "service" <> help "Dienst (AUS/DFI/...)")
                <*> switch (long "invert" <> help "Outputfahrten umdrehen")
                <*> (option exclusionsOpt (long "exclusions" <> help "Welche Tags bei der Ausgabe weggetan werden sollen (kommaseparierte Liste von Pfaden a la <pfad> ist hierbei ein Pfad tagname1/tagname2/...)") <|> pure mempty)

parseSettings :: MonadIO m => m Settings
parseSettings = liftIO (execParser opts)
  where
    opts = info (helper <*> parseSettings')
      (fullDesc <> progDesc "Parsen, filter und analysieren von VDV-Logdateien" <> header "Parsen, filter und analysieren von VDV-Logdateien")

{-# LANGUAGE TemplateHaskell #-}
module Vdv.Filter where

import ClassyPrelude
import Control.Lens(makeLenses)
import qualified Data.Attoparsec.Text as AP
import Vdv.FilterOperator
import Options.Applicative(ReadM,eitherReader)

data Filter = Filter {
    _filterTagName :: Text
  , _filterOperator :: FilterOperator
  , _filterTagValue :: Text
  } deriving(Show,Eq)

$(makeLenses ''Filter)

filterParser :: AP.Parser [Filter]
filterParser = singleFilter  `AP.sepBy` (AP.char ',')
  where parseOperator '=' = FilterOperatorEq
        parseOperator '~' = FilterOperatorLike
        parseOperator o = error $ "Invalid operator " <> show o
        singleFilter = Filter <$> (pack <$> (AP.many1 (AP.satisfy (AP.notInClass "=~")))) <*> (parseOperator <$> AP.anyChar) <*> (pack <$> (AP.many1 (AP.notChar ',')))

parseFilters :: Text -> Either String [Filter]
parseFilters t = AP.parseOnly filterParser t

filtersOpt :: ReadM [Filter]
filtersOpt = eitherReader (parseFilters . pack)

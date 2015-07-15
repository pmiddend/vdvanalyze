{-# LANGUAGE TemplateHaskell #-}
module Vdv.Filter where

import ClassyPrelude
import Control.Lens(makeLenses)
import qualified Data.Attoparsec.Text as AP
import Vdv.FilterOperator
import Vdv.ElementPath
import Vdv.Attoparsec
import Options.Applicative(ReadM,eitherReader)

data Filter = Filter {
    _filterTagPath :: ElementPath
  , _filterOperator :: FilterOperator
  , _filterTagValue :: Text
  } deriving(Show,Eq)

$(makeLenses ''Filter)

operatorChars = "|<>=~"

parseElementPath :: AP.Parser ElementPath
parseElementPath = pathFromParts <$> (manyNotChars ("/," <> operatorChars) `AP.sepBy` (AP.char '/'))

parseOperatorString :: AP.Parser Text
parseOperatorString = pack <$> (AP.many1 (AP.satisfy (AP.inClass operatorChars)))

filterParser :: AP.Parser [Filter]
filterParser = singleFilter  `AP.sepBy` (AP.char ',')
  where parseOperator "=" = FilterOperatorEq
        parseOperator "~" = FilterOperatorLike
        parseOperator "<" = FilterOperatorLe
        parseOperator ">" = FilterOperatorGe
        parseOperator "|<" = FilterOperatorDateLe
        parseOperator "|>" = FilterOperatorDateGe
        parseOperator "|=" = FilterOperatorDateEq
        parseOperator o = error $ "Invalid operator " <> show o
        singleFilter = Filter <$> parseElementPath <*> (parseOperator <$> parseOperatorString) <*> (pack <$> (AP.many1 (AP.notChar ',')))

parseFilters :: Text -> Either String [Filter]
parseFilters t = AP.parseOnly filterParser t

filtersOpt :: ReadM [Filter]
filtersOpt = eitherReader (parseFilters . pack)

module Vdv.Time where

import ClassyPrelude
import Data.Time.LocalTime

isoFormatString :: IsString s => s
isoFormatString = "%Y-%m-%dT%H:%M:%S%Q%z" 

parseISODateToUTC :: Text -> Maybe UTCTime
parseISODateToUTC = parseTime defaultTimeLocale isoFormatString . unpack

formatISODateFromUTC :: UTCTime -> Text
formatISODateFromUTC = pack . formatTime defaultTimeLocale isoFormatString

parseISODateToZoned :: Text -> Maybe ZonedTime
parseISODateToZoned = parseTime defaultTimeLocale isoFormatString . unpack

formatISODateFromZoned :: ZonedTime -> Text
formatISODateFromZoned = pack . formatTime defaultTimeLocale isoFormatString

isoDateOp :: (UTCTime -> UTCTime -> t) -> Text -> Text -> t
isoDateOp op a b = (fromMaybe (error $ "not a valid date \""<> unpack a <>"\"") (parseISODateToUTC a)) `op` (fromMaybe (error $ "not a valid date \""<> unpack b <>"\"") (parseISODateToUTC b))

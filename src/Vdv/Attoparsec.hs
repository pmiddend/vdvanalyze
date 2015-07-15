module Vdv.Attoparsec where

import ClassyPrelude
import qualified Data.Attoparsec.Text as AP

manyNotChars :: String -> AP.Parser Text
manyNotChars cs = pack <$> (AP.many1 (AP.satisfy (AP.notInClass cs)))

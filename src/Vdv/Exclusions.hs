{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vdv.Exclusions(exclusionsOpt,Exclusions,ElementPath,pathElements) where

import ClassyPrelude
import qualified Data.Attoparsec.Text as AP
import Options.Applicative(eitherReader,ReadM)
import Control.Lens(Iso',iso)

newtype ElementPath = ElementPath [Text] deriving(Show,Eq,Read)

newtype Exclusions = Exclusions [ElementPath] deriving(Show,Eq,Read,Monoid)

pathElements :: Iso' ElementPath [Text]
pathElements = iso (\(ElementPath p) -> p) ElementPath

manyNotChars :: String -> AP.Parser Text
manyNotChars cs = pack <$> (AP.many1 (AP.satisfy (AP.notInClass cs)))

parseElementPath :: AP.Parser ElementPath
parseElementPath = ElementPath <$> (manyNotChars "/," `AP.sepBy` (AP.char '/'))

parseExclusions :: Text -> Either String Exclusions
parseExclusions t = AP.parseOnly (Exclusions <$> (parseElementPath `AP.sepBy` (AP.char ','))) t

exclusionsOpt :: ReadM Exclusions
exclusionsOpt = eitherReader (parseExclusions . pack)

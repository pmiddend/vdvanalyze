{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Vdv.Exclusions(exclusionsOpt,Exclusions,ElementPath,foldrExclusion,parseExclusions) where

import ClassyPrelude
import qualified Data.Attoparsec.Text as AP
import Options.Applicative(eitherReader,ReadM)
import Vdv.Attoparsec
import Vdv.ElementPath

newtype Exclusions = Exclusions [ElementPath] deriving(Show,Eq,Read,Monoid)

foldrExclusion :: (ElementPath -> b -> b) -> b -> Exclusions -> b
foldrExclusion f i (Exclusions es) = foldr f i es

parseElementPath :: AP.Parser ElementPath
parseElementPath = pathFromParts <$> (manyNotChars "/," `AP.sepBy` (AP.char '/'))

parseExclusions :: Text -> Either String Exclusions
parseExclusions t = AP.parseOnly (Exclusions <$> (parseElementPath `AP.sepBy` (AP.char ','))) t

exclusionsOpt :: ReadM Exclusions
exclusionsOpt = eitherReader (parseExclusions . pack)

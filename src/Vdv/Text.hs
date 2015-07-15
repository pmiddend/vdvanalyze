module Vdv.Text(extractDelimited,readUnsafeInt) where

import ClassyPrelude
import Data.Text(breakOn)
import Data.Text.Read(decimal)

readUnsafeInt :: Text -> Int
readUnsafeInt x = either (error $ "invalid decimal \"" <> unpack x <> "\"") fst (decimal x)

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

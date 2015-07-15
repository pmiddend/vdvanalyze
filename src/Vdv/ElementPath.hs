module Vdv.ElementPath(ElementPath,pathElements,pathFromParts,foldrElementPath,pathInitUnsafe,pathLastUnsafe) where

import ClassyPrelude hiding (last)
import Control.Lens(Iso',iso)
import Data.List(init,last)

newtype ElementPath = ElementPath [Text] deriving(Show,Eq,Read)

foldrElementPath :: (Text -> b -> b) -> b -> ElementPath -> b
foldrElementPath f i (ElementPath es) = foldr f i es

pathInitUnsafe :: ElementPath -> ElementPath
pathInitUnsafe (ElementPath p) = ElementPath (init p)

pathLastUnsafe :: ElementPath -> Text
pathLastUnsafe (ElementPath p) = last p

pathFromParts :: [Text] -> ElementPath
pathFromParts = ElementPath

pathElements :: Iso' ElementPath [Text]
pathElements = iso (\(ElementPath p) -> p) ElementPath

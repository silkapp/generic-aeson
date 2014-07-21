{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
-- | Helper functions that can be reused by libraries interoperating with generic-aeson.
module Generics.Generic.Aeson.Util
  ( formatLabel
  , multipleConstructors
  , conNameT
  , selNameT
  , module Generics.Generic.IsEnum
  ) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import Generics.Deriving.ConNames
import qualified Data.Text as T

import Generics.Generic.IsEnum

conNameT :: forall c (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a. Constructor c => t c f a -> Text
conNameT x = formatLabel . T.pack . conName $ x

selNameT :: forall s (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a. Selector s => t s f a -> Text
selNameT x = formatLabel . T.pack . selName $ x

-- | Lowercases the first letter and strips leading and trailing underscores.
formatLabel :: Text -> Text
formatLabel = firstLetterToLower
            . stripLeadingAndTrailingUnderscore

stripLeadingAndTrailingUnderscore :: Text -> Text
stripLeadingAndTrailingUnderscore = stripLeadingUnderscore
                                  . stripTrailingUnderscore

stripLeadingUnderscore :: Text -> Text
stripLeadingUnderscore x = maybe x stripLeadingUnderscore $ T.stripPrefix "_" x

stripTrailingUnderscore :: Text -> Text
stripTrailingUnderscore x = fromMaybe x $ T.stripSuffix "_" x

firstLetterToLower :: Text -> Text
firstLetterToLower tx =
  case T.uncons tx of
    Nothing -> ""
    Just (c, t) -> T.cons (toLower c) t

multipleConstructors :: [a] -> Bool
multipleConstructors = (> 1) . length . conNames

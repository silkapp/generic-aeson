{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
-- | Helper functions that can be reused by libraries interoperating with generic-aeson.
module Generics.Aeson.Util
  ( formatLabel
  , multipleConstructors
  , conNameT
  , selNameT
  , module Generics.Aeson.IsEnum
  , Settings (..)
  , defaultSettings
  ) where

import Control.Monad ((<=<))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T

import Generics.Aeson.IsEnum

conNameT :: forall c (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a. Constructor c => Settings -> t c f a -> Text
conNameT set x = formatLabel set . T.pack . conName $ x

selNameT :: forall s (t :: * -> (* -> *) -> * -> *) (f :: * -> *) a. Selector s => Settings -> t s f a -> Maybe Text
selNameT set x = case formatLabel set . T.pack . selName $ x of
  "" -> Nothing
  n  -> Just n

-- | Lowercases the first letter and strips leading and trailing underscores.
formatLabel :: Settings -> Text -> Text
formatLabel set
  = firstLetterToLower
  . stripLeadingAndTrailingUnderscore
  . stripPref set

stripPref :: Settings -> Text -> Text
stripPref set s = (maybe id (\p t -> fromMaybe t . (disallowEmpty <=< T.stripPrefix (T.pack p)) $ t) . stripPrefix) set s
  where
    disallowEmpty x
      | T.null  x = Just s
      | otherwise = Just x

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
multipleConstructors = (> 1) . length

-- Use String over Text so OverloadedStrings isn't necessary
data Settings = Settings { stripPrefix :: Maybe String }
  deriving Show

defaultSettings :: Settings
defaultSettings = Settings { stripPrefix = Nothing }

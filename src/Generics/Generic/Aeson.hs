{-# LANGUAGE
    CPP
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverlappingInstances
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  , TypeOperators
  , TypeSynonymInstances
  , InstanceSigs
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module offers generic conversions to an from JSON 'Value's
-- for data types with a 'Generic' instance.
--
-- The structure of the generated JSON is meant to be close to
-- idiomatic JSON. This means:
--
-- * Enumerations are converted to JSON strings.
--
-- * Record fields become JSON keys.
--
-- * Data types with one unlabeled field convert to just that field.
--
-- * Data types with multiple unlabeled fields become arrays.
--
-- * Multiple constructors are represented by keys.
--
-- * 'Maybe' values are either an absent key, or the value.
--
-- See 'examples/Examples.hs' for more examples.
module Generics.Generic.Aeson
  ( gtoJson
  , gparseJson
  , GtoJson (..)
  , GfromJson (..)
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types hiding (GFromJSON, GToJSON)
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.Text (Text, cons, pack, stripPrefix, stripSuffix, uncons, unpack)
import GHC.Generics
import Generics.Deriving.ConNames
import qualified Data.Vector as V

import Generics.Generic.IsEnum

-- | Class for converting the functors from "GHC.Generics" to JSON.
-- You generally don't need to give any custom instances. Just add
-- 'deriving Generic' and call 'gToJson'.
class GtoJson f where
  -- | Generically show a functor as a JSON value.  The first argument
  -- tells us if there are multiple constructors in the data type. The
  -- second indicates if this data type is an enumeration (only empty
  -- constructors). A functor is then converted to either a list
  -- of values (for non-labeled fields) or a list of String/value
  -- pairs (for labeled fields).
  gtoJSONf :: Bool -> Bool -> f a -> Either [Value] [(Text, Value)]

-- | Class for parsing the functors from "GHC.Generics" from JSON.
-- You generally don't need to give any custom instances. Just add
-- 'deriving Generic' and call 'gFromJson'.
class GfromJson f where
  -- | Generically read a functor from a JSON value.  The first
  -- argument tells us if there are multiple constructors in the data
  -- type. The second indicates if we've already detected that this
  -- data type has multiple constructors. When this is False, the
  -- (:*:) puts the fields in the state. The third indicates if this
  -- data type is an enumeration (only empty constructors). The third
  -- is a function for parsing the recursive positions. A JSON value
  -- is then parsed to either a functor, or a failure.
  gparseJSONf :: Bool -> Bool -> Bool -> StateT [Value] Parser (f a)

-- Void: Used for data types without constructors
-- instance GJSON V1

-- Unit: Used for constructors without arguments
instance GtoJson U1 where
  gtoJSONf _ _ U1 = Right []
instance GfromJson U1 where
  gparseJSONf _ _ _ = return U1

-- | Convert any datatype with a 'Generic' instance to a JSON 'Value'.
gtoJson :: forall a. (Generic a, GtoJson (Rep a), ConNames (Rep a), GIsEnum (Rep a))

        => a -> Value
gtoJson x =
  case gtoJSONf (multipleConstructors x) (isEnum (Proxy :: Proxy a)) (from x) of
    Left  [v] -> v
    Left  _   -> error "The impossible happened: multiple returned values in gtoJSON."
    Right _   -> error "The impossible happened: labeled values returned in gtoJSON."

-- | Parse any datatype with a 'Generic' instance from a JSON 'Value'.
gparseJson :: forall a. (Generic a, GfromJson (Rep a), ConNames (Rep a), GIsEnum (Rep a))
           => Value -> Parser a
gparseJson
  = fmap to
  . evalStateT (gparseJSONf (multipleConstructors (undefined :: a)) False (isEnum (Proxy :: Proxy a)))
  . return

-- Structure type for constant values.
instance (ToJSON c) => GtoJson (K1 a c) where
  gtoJSONf _ _ (K1 a) = Left [toJSON a]
instance (FromJSON c) => GfromJson (K1 a c) where
  gparseJSONf _ _ _   = lift . fmap K1 . parseJSON =<< pop

instance (GtoJson f, GtoJson g) => GtoJson (f :+: g) where
  gtoJSONf mc enm (L1 x) = gtoJSONf mc enm x
  gtoJSONf mc enm (R1 x) = gtoJSONf mc enm x
instance (GfromJson f, GfromJson g) => GfromJson (f :+: g) where
  gparseJSONf mc smf enm  =  L1 <$> gparseJSONf mc smf enm
                        <|> R1 <$> gparseJSONf mc smf enm

instance (GtoJson f, GtoJson g) => GtoJson (f :*: g) where
  gtoJSONf mc enm (x :*: y) =
    case (gtoJSONf mc enm x, gtoJSONf mc enm y) of
      (Left  xvs, Left  yvs) -> Left  (xvs ++ yvs)
      (Right xvs, Right yvs) -> Right (xvs ++ yvs)
      _                      -> error "The impossible happened: product of mixed label and non-label fields in GJSON instance for (:*:)."
instance (GfromJson f, GfromJson g) => GfromJson (f :*: g) where
  gparseJSONf mc smf enm =
    do unless smf selFields
       (:*:) <$> gparseJSONf mc True enm <*> gparseJSONf mc True enm
    where
      selFields =
        do v <- pop
           case v of
             o@Object{} -> put (repeat o)
             Array vs   -> put (V.toList vs)
             _          -> fail "Expected object or array in gparseJSONf for (:*:)."

instance (Selector c, ToJSON a) => GtoJson (M1 S c (K1 i (Maybe a))) where
  gtoJSONf _  _ (M1 (K1 Nothing )) = Right []
  gtoJSONf _  _ (M1 (K1 (Just x))) = Right [(formatLabel . pack $ selName (undefined :: M1 S c f p), toJSON x)]
instance (Selector c, FromJSON a) => GfromJson (M1 S c (K1 i (Maybe a))) where
  gparseJSONf mc smf enm =
    do (M1 (K1 x)) <- gparseJSONf mc smf enm :: StateT [Value] Parser (M1 S c (K1 i a) p)
       return (M1 (K1 (Just x)))
    <|>
    return (M1 (K1 Nothing))

instance GtoJson f => GtoJson (M1 D c f) where
  gtoJSONf a b (M1 x) = gtoJSONf a b x
instance GfromJson f => GfromJson (M1 D c f) where
  gparseJSONf a b x = M1 <$> gparseJSONf a b x

instance (Constructor c, GtoJson f) => GtoJson (M1 C c f) where
  gtoJSONf _  True  (M1 _) = Left [toJSON . formatLabel . pack $ conName (undefined :: M1 C c f p)]
  gtoJSONf mc False (M1 x) =
    case gtoJSONf mc False x of
      -- Single field constructors are not wrapped in an array.
      Left  [v] -> Left [wrap v]
      Left  vs  -> Left [wrap . Array $ V.fromList vs]
      Right vs  -> Left [wrap $ toObject vs]
    where
      wrap = if mc
             then toObject
                . return
                . (formatLabel . pack $ conName (undefined :: M1 C c f p), )
             else id
instance (Constructor c, GfromJson f) => GfromJson (M1 C c f) where
  gparseJSONf mc smf True =
    do str    <- pop
       conStr <- lift (parseJSON str)
       let expectedConStr = formatLabel . pack $ conName (undefined :: M1 C c f p)
       unless (conStr == expectedConStr) $
         fail $ "Error parsing enumeration: expected " ++ unpack expectedConStr ++ ", found " ++ unpack conStr ++ "."
       M1 <$> gparseJSONf mc smf True
  gparseJSONf mc smf False =
    do when mc (selProp "C" propName)
       M1 <$> gparseJSONf mc smf False
    where
      propName = formatLabel . pack $ conName (undefined :: M1 C c f p)

instance (Selector c, GtoJson f) => GtoJson (M1 S c f) where
  gtoJSONf mc enm (M1 x) =
    case gtoJSONf mc enm x of
      Left  [v] -> case formatLabel . pack $ selName (undefined :: M1 S c f p) of
        "" -> Left [v]
        n  -> Right [(n, v)]
      Left  _   -> error "The impossible happened: multiple returned values inside label in GJSON instance for S."
      Right _   -> error "The impossible happened: label inside a label in GJSON instance for S."
instance (Selector c, GfromJson f) => GfromJson (M1 S c f) where
  gparseJSONf mc smf enm =
    do selProp "S" propName
       M1 <$> gparseJSONf mc smf enm
    where
      propName = formatLabel . pack $ selName (undefined :: M1 S c f p)

multipleConstructors :: (Generic a, ConNames (Rep a)) => a -> Bool
multipleConstructors = (> 1) . length . conNames

selProp :: Text -> Text -> StateT [Value] Parser ()
selProp cname propName =
  case propName of
    "" -> do o <- pop
             modify (o:)
    _  -> do o <- pop
             v <- lift (withObject ("Expected property " ++ show propName ++ " in object in gparseJSONf for " ++ show cname ++ ".")
                                   (.: propName) o)
             modify (v:)

pop :: MonadState [Value] m => m Value
pop =
  do (v:vs) <- get
     put vs
     return v

formatLabel :: Text -> Text
formatLabel = id firstLetterToLower
            . stripLeadingAndTrailingUnderscore

stripLeadingAndTrailingUnderscore :: Text -> Text
stripLeadingAndTrailingUnderscore = stripLeadingUnderscore
                                  . stripTrailingUnderscore

stripLeadingUnderscore :: Text -> Text
stripLeadingUnderscore x = maybe x stripLeadingUnderscore $ stripPrefix "_" x

stripTrailingUnderscore :: Text -> Text
stripTrailingUnderscore x = fromMaybe x $ stripSuffix "_" x

firstLetterToLower :: Text -> Text
firstLetterToLower tx =
  case uncons tx of
    Nothing -> ""
    Just (c, t) -> cons (toLower c) t

toObject :: ToJSON v => [(Text, v)] -> Value
toObject = object . map (uncurry (.=))

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

module Generics.Generic.Aeson (GJSON (..), gtoJSON, gparseJSON) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.Text (Text, cons, pack, stripPrefix, stripSuffix, uncons, unpack)
import GHC.Generics
import Generics.Deriving.ConNames
import qualified Data.Vector as V

import Generics.Generic.IsEnum

class GJSON f where
  -- | Generically show a functor as a JSON value.  The first argument
  -- tells us if there are multiple constructors in the data type. The
  -- second indicates if this data type is an enumeration (only empty
  -- constructors). A functor is then converted to either a list
  -- of values (for non-labeled fields) or a list of String/value
  -- pairs (for labeled fields).
  gtoJSONf :: Bool -> Bool -> f a -> Either [Value] [(Text, Value)]
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
instance GJSON U1 where
  gtoJSONf _ _ U1 = Right []
  gparseJSONf _ _ _ = return U1

gtoJSON :: forall a. (Generic a, GJSON (Rep a), ConNames (Rep a), GIsEnum (Rep a))
        => a -> Value
gtoJSON x =
  case gtoJSONf (multipleConstructors x) (isEnum (Proxy :: Proxy a)) (from x) of
    Left  [v] -> v
    Left  _   -> error "The impossible happened: multiple returned values in gtoJSON."
    Right _   -> error "The impossible happened: labeled values returned in gtoJSON."

gparseJSON :: forall a. (Generic a, GJSON (Rep a), ConNames (Rep a), GIsEnum (Rep a))
           => Value -> Parser a
gparseJSON
  = fmap to
  . evalStateT (gparseJSONf (multipleConstructors (undefined :: a)) False (isEnum (Proxy :: Proxy a)))
  . return

-- Structure type for constant values.
instance (ToJSON c, FromJSON c) => GJSON (K1 a c) where
  gtoJSONf _ _ (K1 a) = Left [toJSON a]
  gparseJSONf _ _ _   = lift . fmap K1 . parseJSON =<< pop

instance (GJSON f, GJSON g) => GJSON (f :+: g) where
  gtoJSONf mc enm (L1 x) = gtoJSONf mc enm x
  gtoJSONf mc enm (R1 x) = gtoJSONf mc enm x
  gparseJSONf mc smf enm  =  L1 <$> gparseJSONf mc smf enm
                        <|> R1 <$> gparseJSONf mc smf enm

instance (GJSON f, GJSON g) => GJSON (f :*: g) where
  gtoJSONf mc enm (x :*: y) =
    case (gtoJSONf mc enm x, gtoJSONf mc enm y) of
      (Left  xvs, Left  yvs) -> Left  (xvs ++ yvs)
      (Right xvs, Right yvs) -> Right (xvs ++ yvs)
      _                      -> error "The impossible happened: product of mixed label and non-label fields in GJSON instance for (:*:)."
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

instance (Selector c, ToJSON a, FromJSON a) => GJSON (M1 S c (K1 i (Maybe a))) where
  gtoJSONf _  _ (M1 (K1 Nothing )) = Right []
  gtoJSONf _  _ (M1 (K1 (Just x))) = Right [(formatLabel . pack $ selName (undefined :: M1 S c f p), toJSON x)]
  gparseJSONf mc smf enm =
    -- We know the undefined here is never used. We could give it in
    -- terms of 'f' and 'fromJust', but that requires a type signature
    -- on this instance of gparseJSONf, which is annoying since you
    -- can't easily give type signatures on instance methods.
    do (M1 (K1 x)) <- gparseJSONf mc smf enm :: StateT [Value] Parser (M1 S c (K1 i a) p)
       return (M1 (K1 (Just x)))
    <|>
    return (M1 (K1 Nothing))

instance GJSON f => GJSON (M1 D c f) where
  gtoJSONf a b (M1 x) = gtoJSONf a b x
  gparseJSONf a b x = M1 <$> gparseJSONf a b x

instance (Constructor c, GJSON f) => GJSON (M1 C c f) where
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

instance (Selector c, GJSON f) => GJSON (M1 S c f) where
  gtoJSONf mc enm (M1 x) =
    case gtoJSONf mc enm x of
      Left  [v] -> case formatLabel . pack $ selName (undefined :: M1 S c f p) of
        "" -> Left [v]
        n  -> Right [(n, v)]
      Left  _   -> error "The impossible happened: multiple returned values inside label in GJSON instance for S."
      Right _   -> error "The impossible happened: label inside a label in GJSON instance for S."
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

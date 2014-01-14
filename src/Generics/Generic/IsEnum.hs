{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , TypeOperators
  #-}
module Generics.Generic.IsEnum where

import Data.Proxy

import GHC.Generics

class GIsEnum f where
  gIsEnum :: Proxy (f a) -> Bool


instance GIsEnum V1 where
  gIsEnum _ = False

instance GIsEnum (K1 i a) where
  gIsEnum _ = False

instance GIsEnum U1 where
  gIsEnum _ = True

instance GIsEnum Par1 where
  gIsEnum _ = False

-- should be K1 R
instance GIsEnum (Rec1 f) where
  gIsEnum _ = False


instance (GIsEnum f, GIsEnum g) => GIsEnum (f :+: g) where
  gIsEnum _ = gIsEnum (Proxy :: Proxy (f a)) && gIsEnum (Proxy :: Proxy (g a))

instance (GIsEnum f, GIsEnum g) => GIsEnum (f :*: g) where
  gIsEnum _ = False

instance GIsEnum f => GIsEnum (M1 C c f) where
  gIsEnum _ = gIsEnum (Proxy :: Proxy (f a))

instance GIsEnum (M1 S c a) where
  gIsEnum _ = False

instance GIsEnum f => GIsEnum (M1 D c f) where
  gIsEnum _ = gIsEnum (Proxy :: Proxy (f a))

-- instance GIsEnum (f :.: g) where

isEnum :: forall a. (Generic a, GIsEnum (Rep a)) => Proxy a -> Bool
isEnum _ = gIsEnum (Proxy :: Proxy ((Rep a) a))

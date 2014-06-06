{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , TemplateHaskell
  , TypeFamilies
  #-}
module Main (main) where

import Data.Aeson hiding (Result)
import Data.Aeson.Parser
import Data.Attoparsec.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import GHC.Generics (Generic)
import Generics.Generic.Aeson
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Data.Aeson.Types as A

data SingleCons = SingleCons deriving (Generic, Show, Eq)
instance ToJSON   SingleCons where toJSON    = gtoJson
instance FromJSON SingleCons where parseJSON = gparseJson
case_constructorWithoutFields =
  (unsafeParse "\"singleCons\"", Right SingleCons)
    @=?
  (toJSON SingleCons , encDec SingleCons)

data Record = Record { field :: Int } deriving (Generic, Show, Eq)
instance ToJSON   Record where toJSON    = gtoJson
instance FromJSON Record where parseJSON = gparseJson
case_record =
  (unsafeParse "{\"field\":1}"          , Right (Record { field = 1 }))
    @=?
  (toJSON Record { field = 1 }, encDec Record { field = 1 })

data RecordTwoFields = D { d1 :: Int, d2 :: String } deriving (Generic, Show, Eq)
instance ToJSON   RecordTwoFields where toJSON = gtoJson
instance FromJSON RecordTwoFields where parseJSON = gparseJson
case_recordWithFields =
  (unsafeParse "{\"d1\":1,\"d2\":\"aap\"}"  , Right (D {d1 = 1, d2 = "aap"}))
    @=?
  (toJSON D { d1 = 1, d2 = "aap" }, encDec D { d1 = 1, d2 = "aap" })

data E = E Int deriving (Generic, Show, Eq)
instance ToJSON   E where toJSON = gtoJson
instance FromJSON E where parseJSON = gparseJson
case_constructorOneField =
  (unsafeParse "1"        , Right (E 1))
    @=?
  (toJSON (E 1) , encDec (E 1))

data F = F Int String deriving (Generic, Show, Eq)
instance ToJSON   F where toJSON = gtoJson
instance FromJSON F where parseJSON = gparseJson
case_constructorWithFields =
  (unsafeParse "[1,\"aap\"]",Right (F 1 "aap"))
    @=?
  (toJSON (F 1 "aap"), encDec (F 1 "aap"))

data G = G1 Int | G2 String deriving (Generic, Show, Eq)
instance ToJSON   G where toJSON = gtoJson
instance FromJSON G where parseJSON = gparseJson
case_sumConstructorsWithField =
  (unsafeParse "{\"g1\":1}",unsafeParse "{\"g2\":\"aap\"}",Right (G1 1),Right (G2 "aap"))
    @=?
  (toJSON (G1 1), toJSON (G2 "aap"), encDec (G1 1), encDec (G2 "aap"))

data H = H1 { h1 :: Int } | H2 { h2 :: String } deriving (Generic, Show, Eq)
instance ToJSON   H where toJSON = gtoJson
instance FromJSON H where parseJSON = gparseJson
case_sumRecord =
  (unsafeParse "{\"h1\":{\"h1\":1}}",unsafeParse "{\"h2\":{\"h2\":\"aap\"}}",Right (H1 {h1 = 1}),Right (H2 {h2 = "aap"}))
    @=?
  (toJSON (H1 1), toJSON (H2 "aap"), encDec (H1 1), encDec (H2 "aap"))

data J = J1 { j1 :: Int, j2 :: String } | J2 deriving (Generic, Show, Eq)
instance ToJSON   J where toJSON = gtoJson
instance FromJSON J where parseJSON = gparseJson
case_sumRecordConstructorWithoutFields =
  (unsafeParse "{\"j1\":{\"j1\":1,\"j2\":\"aap\"}}",unsafeParse "{\"j2\":{}}",Right (J1 {j1 = 1, j2 = "aap"}),Right J2)
    @=?
  (toJSON (J1 1 "aap"), toJSON J2, encDec (J1 1 "aap"), encDec J2)

data L = L1 | L2 Int String deriving (Generic, Show, Eq)
instance ToJSON   L where toJSON = gtoJson
instance FromJSON L where parseJSON = gparseJson
case_sumConstructorWithoutFieldsConstructorWithFields =
  (unsafeParse "{\"l1\":{}}",unsafeParse "{\"l2\":[1,\"aap\"]}",Right L1,Right (L2 1 "aap"))
    @=?
  (toJSON L1, toJSON (L2 1 "aap"), encDec L1, encDec (L2 1 "aap"))

data M = M1 | M2 Int M deriving (Generic, Show, Eq)
instance ToJSON   M where toJSON = gtoJson
instance FromJSON M where parseJSON = gparseJson
case_sumConstructorWithoutFieldsConstructorWithRecursiveField =
  (unsafeParse "{\"m1\":{}}",unsafeParse "{\"m2\":[1,{\"m1\":{}}]}",unsafeParse "{\"m2\":[1,{\"m2\":[2,{\"m1\":{}}]}]}",Right M1,Right (M2 1 M1),Right (M2 1 (M2 2 M1)))
    @=?
  (toJSON M1, toJSON (M2 1 M1), toJSON (M2 1 (M2 2 M1)), encDec M1, encDec (M2 1 M1), encDec (M2 1 (M2 2 M1)))

data N = N1 | N2 { n1 :: Int, n2 :: N } deriving (Generic, Show, Eq)
instance ToJSON   N where toJSON = gtoJson
instance FromJSON N where parseJSON = gparseJson
case_sum_constructorWithoutFields_record =
  (unsafeParse "{\"n1\":{}}",unsafeParse "{\"n2\":{\"n2\":{\"n1\":{}},\"n1\":1}}",unsafeParse "{\"n2\":{\"n1\":1,\"n2\":{\"n2\":{\"n1\":2,\"n2\":{\"n1\":{}}}}}}",Right N1,Right (N2 {n1 = 1, n2 = N1}),Right (N2 {n1 = 1, n2 = N2 {n1 = 2, n2 = N1}}))
    @=?
  (toJSON N1, toJSON (N2 1 N1), toJSON (N2 1 (N2 2 N1)), encDec N1, encDec (N2 1 N1), encDec (N2 1 (N2 2 N1)))

data O = O { o :: [Int] } deriving (Generic, Show, Eq)
instance ToJSON   O where toJSON = gtoJson
instance FromJSON O where parseJSON = gparseJson
case_recordListField =
  (unsafeParse "{\"o\":[1,2,3]}",Right (O {o = [1,2,3]}))
    @=?
  (toJSON (O [1,2,3]), encDec (O [1,2,3]))

data P = P [Int] deriving (Generic, Show, Eq)
instance ToJSON   P where toJSON = gtoJson
instance FromJSON P where parseJSON = gparseJson
case_constructorListField =
  (unsafeParse "[1,2,3]",Right (P [1,2,3]))
    @=?
  (toJSON (P [1,2,3]), encDec (P [1,2,3]))

data Q = Q Int Int Int deriving (Generic, Show, Eq)
instance ToJSON   Q where toJSON = gtoJson
instance FromJSON Q where parseJSON = gparseJson
case_ConstructorSameTypedFields =
  (unsafeParse "[1,2,3]",Right (Q 1 2 3))
    @=?
  (toJSON (Q 1 2 3), encDec (Q 1 2 3))

data T = T { r1 :: Maybe Int } deriving (Generic, Show, Eq)
instance ToJSON   T where toJSON = gtoJson
instance FromJSON T where parseJSON = gparseJson
case_RecordMaybeField =
  (unsafeParse "{}", unsafeParse "{\"r1\":1}",Right (T {r1 = Nothing}),Right (T {r1 = Just 1}))
    @=?
  (toJSON (T Nothing), toJSON (T (Just 1)), encDec (T Nothing), encDec (T (Just 1)))

data V = V1 | V2 | V3 deriving (Generic, Show, Eq)
instance ToJSON   V where toJSON = gtoJson
instance FromJSON V where parseJSON = gparseJson
case_constructorsWithoutFields =
  (unsafeParse "\"v1\"",unsafeParse "\"v2\"",Right V1,Right V2)
    @=?
  (toJSON V1, toJSON V2, encDec V1, encDec V2)

data W = W { underscore1_ :: Int, _underscore2 :: Int } deriving (Generic, Show, Eq)
instance ToJSON   W where toJSON = gtoJson
instance FromJSON W where parseJSON = gparseJson
case_recordWithUnderscoredFields =
  (unsafeParse "{\"underscore1\":1,\"underscore2\":2}",Right (W {underscore1_ = 1, _underscore2 = 2}))
    @=?
  (toJSON (W 1 2), encDec (W 1 2))

-- Helpers

encDec :: (FromJSON a, ToJSON a) => a -> Either String a
encDec a = case (parse value . encode) a of
  Done _ r -> case fromJSON r of A.Success v -> Right v; Error s -> Left $ "fromJSON r=" ++ show r ++ ", s=" ++ s
  Fail _ ss e -> Left . concat $ intersperse "," (ss ++ [e])

unsafeParse :: ByteString -> Value
unsafeParse = fromResult . parse value

fromResult (Done _ r) = r
fromResult _ = error "Boo"

tests :: TestTree
tests = $testGroupGenerator

main :: IO ()
main = defaultMain $ testGroup "generic-aeson" [tests]

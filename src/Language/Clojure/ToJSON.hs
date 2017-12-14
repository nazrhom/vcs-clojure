{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Clojure.ToJSON where

import Data.Aeson
import Data.Text
import qualified Data.Vector as V

import Language.Clojure.AST
import Language.Clojure.Lang


instance ToJSON Expr where
  toJSON (Special fty expr _) = object [
    "text" .= object [
        "name" .= ("Special" :: Text)
      , "type" .= show fty ]
    , "children" .= Array (V.singleton (toJSON expr)) ]
  toJSON (Dispatch expr _) = object [
    "text" .= object [
      "name" .= ("Dispatch" :: Text) ]
    , "children" .= Array (V.singleton (toJSON expr)) ]
  toJSON (Collection cty sel _) = object [
    "text" .= object [
        "name" .= ("Collection" :: Text)
      , "type" .= show cty ]
    , "children" .= Array (V.singleton (toJSON sel)) ]
  toJSON (Term term _) = object [
    "text" .= object [
      "name" .= ("Term" :: Text) ]
    , "children" .= Array (V.singleton (toJSON term)) ]
  toJSON (Comment str _) = object [
    "text" .= object [
      "name" .= ("Comment" :: Text)
    , "value" .= show str ] ]
  toJSON (Seq e1 e2 _) = object [
    "text" .= object [
      "name" .= ("Seq" :: Text) ]
    , "children" .= Array ((toJSON e1) `V.cons` (V.singleton (toJSON e2))) ]
  toJSON (Empty _) = object [
    "text" .= object [
      "name" .= ("Empty" :: Text) ] ]

instance ToJSON SepExprList where
  toJSON (Nil _) =  object [
    "text" .= object [
      "name" .= ("Nil" :: Text) ] ]
  toJSON (Cons expr sep sel _) = object [
    "text" .= object [
        "name" .= ("Cons" :: Text)
      , "type" .= show sep ]
    , "children" .= Array ((toJSON expr) `V.cons` (V.singleton (toJSON sel))) ]

instance ToJSON Term where
  toJSON (TaggedString tag str _) = object [
    "text" .= object [
        "name" .= ("TaggedString" :: Text)
      , "type" .= show tag
      , "value" .= show str ] ]

instance ToJSON LineRange where
  toJSON (Range start end) = object [ "text" .= object [
      "name" .= ("Range" :: Text)
    , "start" .= toJSON start
    , "end" .= toJSON end ] ]

instance ToJSON (Usingl u) where
  toJSON (UString u) = fromShowable u
  toJSON (USep u) = fromShowable u
  toJSON (UCollTy u) = fromShowable u
  toJSON (UFormTy u) = fromShowable u
  toJSON (UTag u) = fromShowable u
  toJSON (USepExprList u) = toJSON u
  toJSON (UExpr u) = toJSON u
  toJSON (UTerm u) = toJSON u

fromShowable :: Show s => s -> Value
fromShowable u = object [ "text" .= object ["value" .= show u ] ]
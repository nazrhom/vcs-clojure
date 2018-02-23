{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Presentation.ToJSON where

import Data.Aeson
import Data.Text
import qualified Data.Vector as V

import Language.Presentation.AST
import Language.Presentation.Lang

instance ToJSON T where
  toJSON (Sum t1 t2) = object [
    "text" .=  object [
      "name" .= ("Sum" :: Text)
    ], "children" .= Array (toJSON t1 `V.cons` (V.singleton (toJSON t2))) ]
  toJSON (Atom a) = object [
    "text" .=  object [
      "name" .= ("Atom" :: Text)
    ], "children" .= Array (V.singleton (toJSON a)) ]

instance ToJSON A where
  toJSON (Parens t) = object [
    "text" .=  object [
      "name" .= ("Parens" :: Text)
    ], "children" .= Array (V.singleton (toJSON t)) ]
  toJSON (Val i) = object [
    "text" .=  object [
      "name" .= ("Val" :: Text)
    , "value" .= show i ]]

instance ToJSON (Usingl u) where
  toJSON (UInt i) = object [ "text" .= object ["value" .= show i] ]
  toJSON (UT t) = toJSON t
  toJSON (UA a) = toJSON a
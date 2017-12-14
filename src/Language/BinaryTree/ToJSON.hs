{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.BinaryTree.ToJSON where

import Data.Aeson
import Data.Text
import qualified Data.Vector as V

import Language.BinaryTree.AST
import Language.BinaryTree.Lang

instance ToJSON IntTree where
  toJSON (Node t1 t2) = object [
    "text" .=  object [
      "name" .= ("Node" :: Text)
    ], "children" .= Array (toJSON t1 `V.cons` (V.singleton (toJSON t2))) ]
  toJSON (Leaf i)    = object [
    "text" .= object [ "value" .= show i ]]
instance ToJSON (Usingl u) where
  toJSON (UInt i)     = object [ "text" .= object ["value" .= show i] ]
  toJSON (UIntTree u) = toJSON u
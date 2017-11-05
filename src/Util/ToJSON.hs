{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.ToJSON where

import VCS.Multirec
import Clojure.Lang
import Clojure.AST

import Data.Aeson
import Data.Text
import qualified Data.Vector as V

mkNode :: Object -> V.Vector Value -> Value
mkNode obj children = object [ "text" .= obj
                             , "children" .= Array children ]

mkLeaf :: Object -> Value
mkLeaf obj = mkNode obj V.empty

toJSONArray :: Value -> Value
toJSONArray val = Array (V.singleton val)

instance ToJSON (Spine (At AlmuH) (Al (At AlmuH)) u) where
  toJSON Scp = object [ "text" .= object [ "name" .= ("Scp" :: Text) ] ]
  toJSON (Scns c p) = object [
      "text" .= object [ "name" .= ("Scns" :: Text)
                       , "value" .= showConstr c ]
    , "children" .= toJSON p ]
  toJSON (Schg i j p) = object [
      "text" .= object [ "name" .= ("Schg" :: Text)
                       , "from" .= showConstr i
                       , "to" .= showConstr j ]
    , "children" .= toJSON p ]

instance ToJSON (Almu u v) where
  toJSON (Alspn s) = object [
      "text" .= object [ "name" .= ("Alspn" :: Text) ]
    , "children" .= toJSONArray (toJSON s) ]
  toJSON (Aldel d ctx) = object [
      "text" .= object [ "name" .= ("Aldel" :: Text)
                       , "value" .= show d ]
    , "children" .= toJSON ctx]
  toJSON (Alins i ctx) = object [
      "text" .= object [ "name" .= ("Alins" :: Text)
                       , "value" .= show i ]
    , "children" .= toJSON ctx ]

instance ToJSON (Al (At AlmuH) p1 p2) where
  toJSON al = Array $ go al
    where go :: (Al (At AlmuH) p1 p2) -> V.Vector Value
          go A0 = V.empty
          go (Ains i al) = (object [
              "text" .= object ["name" .= ("Ains" :: Text)]
            , "children" .= toJSONArray (toJSON i) ])
            `V.cons` go al
          go (Adel d al) = (object [
              "text" .= object ["name" .= ("Adel" :: Text)]
            , "children" .= toJSONArray (toJSON d) ])
            `V.cons` go al
          go (Amod m al) = (object [
              "text" .= object ["name" .= ("Amod" :: Text)]
            , "children" .= toJSONArray (toJSON m) ])
            `V.cons` go al

instance ToJSON (Ctx (AtmuPos u) v) where
  toJSON ctx = Array $ go ctx
    where
      go :: (Ctx (AtmuPos u) v) -> V.Vector Value
      go (Here r p) = value `V.cons` rest
        where
          value = object [ "text" .= object [
                              "name"  .= ("Here" :: Text) ]
                          , "children" .= toJSONArray (toJSON r ) ]
          (Array rest) = toJSON p

      go (There u p) = value `V.cons` go p
        where
          value = object [ "text" .= object [
                              "name" .= ("There" :: Text) ]
                          , "children" .= toJSONArray (toJSON u) ]

instance ToJSON (Ctx (AtmuNeg u) v) where
  toJSON ctx = Array $ go ctx
    where
      go :: (Ctx (AtmuNeg u) v) -> V.Vector Value
      go (Here r p) = value `V.cons` rest
        where
          value = object [ "text" .= object [
                              "name"  .= ("Here" :: Text) ]
                          , "children" .= toJSONArray (toJSON r) ]
          (Array rest) = toJSON p

      go (There u p) = value `V.cons` go p
        where
          value = object [ "text" .= object [
                              "name" .= ("There" :: Text) ]
                          , "children" .= toJSONArray (toJSON u) ]

instance ToJSON (At AlmuH l) where
  toJSON (Ai r) = toJSON r
  toJSON (As t) = toJSON t

instance ToJSON (AlmuH u) where
instance ToJSON (AtmuPos u v) where
instance ToJSON (AtmuNeg u v) where
instance ToJSON (ConstrFor u v) where
  toJSON c = object [ "text" .= object [ "value" .= showConstr c ]]
instance ToJSON (Contract Usingl l) where
  toJSON c = if old == new
             then object [ "text" .= object [
                              "value" .= show new ] ]
             else object [ "text" .= object [
                              "src" .= show old
                            , "dst" .= show new ] ]
      where
        (old, new) = unContract c

instance ToJSON Expr where
  toJSON (Special fty expr _) = object [
    "text" .= object [
        "name" .= ("Special" :: Text)
      , "type" .= show fty ]
    , "children" .= toJSONArray (toJSON expr) ]
  toJSON (Dispatch expr _) = object [
    "text" .= object [
      "name" .= ("Dispatch" :: Text) ]
    , "children" .= toJSONArray (toJSON expr) ]
  toJSON (Collection cty sel _) = object [
    "text" .= object [
        "name" .= ("Collection" :: Text)
      , "type" .= show cty ]
    , "children" .= toJSONArray (toJSON sel) ]
  toJSON (Term term _) = object [
    "text" .= object [
      "name" .= ("Term" :: Text) ]
    , "children" .= toJSONArray (toJSON term) ]
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

instance ToJSON (All (At AlmuH) l) where
  toJSON as = Array $ go as
    where
        go :: All (At AlmuH) l -> V.Vector Value
        go An = V.empty
        go (b `Ac` bs) = toJSON b `V.cons` go bs

instance ToJSON (All Usingl l) where
  toJSON as = Array $ go as
    where
        go :: All Usingl l -> V.Vector Value
        go An = V.empty
        go (b `Ac` bs) = toJSON b `V.cons` go bs

instance ToJSON (Usingl u) where
  toJSON (UString u) = object [ "text" .= object ["values" .= show u ] ]
  toJSON (USepExprList u) = toJSON u
  toJSON (UExpr u) = toJSON u
  toJSON (UTerm u) = toJSON u
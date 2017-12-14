{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.ToJSON where

import VCS.Multirec

import Language.Clojure.Lang
import Language.Clojure.PrettyPrint (ppConstr)
import Language.Clojure.ToJSON
import Language.Common

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
                       , "value" .= show (ppConstr c) ]
    , "children" .= toJSON p ]
  toJSON (Schg i j p) = object [
      "text" .= object [ "name" .= ("Schg" :: Text)
                       , "from" .= show (ppConstr i)
                       , "to" .= show (ppConstr j) ]
    , "children" .= toJSON p ]

instance ToJSON (Almu u v) where
  toJSON (Alspn s) = object [
      "text" .= object [ "name" .= ("Alspn" :: Text) ]
    , "children" .= toJSONArray (toJSON s) ]
  toJSON (Aldel d ctx) = object [
      "text" .= object [ "name" .= ("Aldel" :: Text)
                       , "value" .= show (ppConstr d) ]
    , "children" .= toJSON ctx]
  toJSON (Alins i ctx) = object [
      "text" .= object [ "name" .= ("Alins" :: Text)
                       , "value" .= show (ppConstr i) ]
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
  toJSON c = object [ "text" .= object [ "value" .= show (ppConstr c) ]]

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

instance ToJSON (Contract Usingl l) where
  toJSON c = if old == new
             then object [ "text" .= object [
                              "value" .= show new ] ]
             else object [ "text" .= object [
                              "src" .= show old
                            , "dst" .= show new ] ]
      where
        (old, new) = unContract c


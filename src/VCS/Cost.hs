{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module VCS.Cost where

import VCS.Multirec
import Language.Clojure.Lang
import Language.Clojure.Cost
import Language.Common

costS :: (forall a . at a -> Int)
      -> (forall p1 p2 . al p1 p2 -> Int)
      -> Spine at al u -> Int
costS costAt costAl Scp = 0
costS costAt costAl (Scns c p) = sumAll costAt p
costS costAt costAl (Schg i j p) = costAl p

sumAll :: (forall a . at a -> Int) -> All at p -> Int
sumAll costAt An = 0
sumAll costAt (a `Ac` as) = costAt a + sumAll costAt as

costAl :: (forall a . at a -> Int)
       -> Al at p1 p2 -> Int
costAl costAt A0 = 0
costAl costAt (Adel a al) = costUsingl a + costAl costAt al
costAl costAt (Ains a al) = costUsingl a + costAl costAt al
costAl costAt (Amod at al) = costAt at + costAl costAt al

costAt :: (IsRecEl a => rec a -> Int)
       -> At rec a -> Int
costAt costR (As pair) = costK pair
costAt costR (Ai spmu) = costR spmu

costK :: TrivialA u -> Int
costK c = if old == new then 0 else 2
  where (old, new) = unContract c

costAlmu :: Almu v u -> Int
costAtmuPos :: AtmuPos v u -> Int
costAtmuNeg :: AtmuNeg v u -> Int
costAlmuH :: AlmuH u -> Int

costAlmu (Alspn sp) = costS (costAt costAlmuH) (costAl (costAt costAlmuH)) sp
costAlmu (Alins c ctx) = 1 + costCtxPos ctx
costAlmu (Aldel c ctx) = 1 + costCtxNeg ctx

costAtmuPos (FixPos almu) = costAlmu almu
costAtmuNeg (FixNeg almu) = costAlmu almu
costAlmuH   (AlmuH almu)  = costAlmu almu

costCtxPos :: Ctx (AtmuPos v) l -> Int
costCtxPos (Here spmu atmus) = costAtmuPos spmu + costAll atmus
costCtxPos (There atmu almu) = costUsingl atmu + costCtxPos almu

costCtxNeg :: Ctx (AtmuNeg v) l -> Int
costCtxNeg (Here spmu atmus) = costAtmuNeg spmu + costAll atmus
costCtxNeg (There atmu almu) = costUsingl atmu + costCtxNeg almu

costAll :: All Usingl l -> Int
costAll as = foldAll (\u acc -> acc + costUsingl u) 0 as

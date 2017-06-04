{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Cost where

import Multirec
import Lang


costS :: (forall a . at a -> Int)
      -> (forall p1 p2 . al p1 p2 -> Int)
      -> Spine at al u -> Int
costS costAt costAl Scp = 0
costS costAt costAl (Scns c p) = sumAll costAt p
  where
    sumAll :: (forall a . at a -> Int) -> All at p -> Int
    sumAll costAt An = 0
    sumAll costAt (a `Ac` as) = costAt a + sumAll costAt as
costS costAt costAl (Schg i j p) = costAl p

costAl :: (forall a . at a -> Int)
       -> Al at p1 p2 -> Int
costAl costAt A0 = 0
costAl costAt (Adel a al) = 1 + costAl costAt al
costAl costAt (Ains a al) = 1 + costAl costAt al
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
costAlmu (Alins c ctx) = costCtxPos ctx
costAlmu (Aldel c ctx) = costCtxNeg ctx

costAtmuPos (FixPos almu) = costAlmu almu
costAtmuNeg (FixNeg almu) = costAlmu almu
costAlmuH   (AlmuH almu)  = costAlmu almu

costCtxPos :: Ctx (AtmuPos v) l -> Int
costCtxPos (Here spmu atmus) = costAtmuPos spmu + lengthAll atmus
costCtxPos (There atmu almu) = 1 + costCtxPos almu

costCtxNeg :: Ctx (AtmuNeg v) l -> Int
costCtxNeg (Here spmu atmus) = costAtmuNeg spmu + lengthAll atmus
costCtxNeg (There atmu almu) = 1 + costCtxNeg almu

lengthAll :: All p l -> Int
lengthAll as = foldAll (const (+1)) 0 as

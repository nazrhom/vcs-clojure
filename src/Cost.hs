{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Cost where

import Multirec
import Lang


costS :: (forall a . at a -> Int)
      -> (forall p1 p2 . al p1 p2 -> Int)
      -> Spine at al u -> Int
costS costAt costAl Scp = 0
costS costAt costAl (Scns c p) = sumAll costAt p
  where
    sumAll :: (forall a . at a -> Int) -> All (at :: U -> *) p -> Int
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

costK :: Trivial Usingl u -> Int
costK c = if old == new then 0 else 2
  where (old, new) = unContract c

costAlmu :: Almu v u -> Int
costAtmuPos :: AtmuPos v u -> Int
costAtmuNeg :: AtmuNeg v u -> Int
costAlmuH ::  AlmuH u -> Int

costAlmu (Alspn sp) = costS (costAt costAlmuH) (costAl (costAt costAlmuH)) sp
costAlmu (Alins c ctx) = foldCtx (\atmu acc -> costAtmuPos atmu + acc) (const (+1)) 0 ctx
costAlmu (Aldel c ctx) = foldCtx (\atmu acc -> costAtmuNeg atm + acc) (const (+1)) 0 ctx

costAtmuPos (FixPos almu) = costAlmu almu
costAtmuNeg (FixNeg almu) = costAlmu almu
costAlmuH   (AlmuH almu)  = costAlmu almu

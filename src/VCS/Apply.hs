{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}


module VCS.Apply where

import Data.Type.Equality hiding (apply)

import VCS.Multirec
import Language.Clojure.Lang
import Language.Common

import Debug.Trace

applyS :: IsRecEl r => (forall a . at a -> Usingl a -> Maybe (Usingl a))
       -> (forall p1 p2 . al p1 p2 -> All Usingl p1 -> Maybe (All Usingl p2))
       -> Spine at al r
       -> Usingl r
       -> Maybe (Usingl r)
applyS appAt appAl Scp x = pure x
applyS appAt appAl (Schg i j p) x = case view x of
  Tag c d -> do
    Refl <- testEquality c i
    inj j <$> appAl p d
applyS appAt appAl (Scns i p) x = case view x of
  Tag c d -> do
    Refl <- testEquality c i
    inj i <$> sAll appAt p d
  where
    sAll :: (forall a . at a -> Usingl a -> Maybe (Usingl a))
         -> All at p -> All Usingl p -> Maybe (All Usingl p)
    sAll appAt An            An          = pure An
    sAll appAt (at `Ac` ats) (a `Ac` as) = Ac <$> appAt at a <*> sAll appAt ats as

applyAl :: (forall a . at a -> Usingl a -> Maybe (Usingl a))
        -> Al at p1 p2 -> All Usingl p1 -> Maybe (All Usingl p2)
applyAl appAt A0 An
  = pure An
applyAl appAt (Amod p a) (Ac x xs)
  = Ac <$> appAt p x <*> applyAl appAt a xs
applyAl appAt (Ains k a) xs
  = Ac <$> pure k <*> applyAl appAt a xs
applyAl appAt (Adel k a) (Ac x xs) = do
  Refl <- testEquality x k
  applyAl appAt a xs


applyAt :: (IsRecEl a => rec a -> Usingl a -> Maybe (Usingl a))
        -> At rec a -> Usingl a -> Maybe (Usingl a)
applyAt appRec (Ai r) x = appRec r x
applyAt appRec (As c) x = if old == new then pure x
                          else if old == x then pure new
                          else trace "applyAt failing" Nothing
    where (old, new) = unContract c

applyAtAlmuH :: At AlmuH a -> Usingl a -> Maybe (Usingl a)
applyAtAlmuH = applyAt (unH applyAlmu)
  where
    unH f (AlmuH al) x = f al x

ctxIns :: IsRecEl u => Ctx (AtmuPos u) l -> Usingl u -> Maybe (All Usingl l)
ctxIns (Here (FixPos almu) xs)  u = Ac <$> applyAlmu almu u <*> pure xs
ctxIns (There x xs) u = Ac <$> pure x <*> ctxIns xs u

ctxDel :: IsRecEl v => Ctx (AtmuNeg v) l -> All Usingl l -> Maybe (Usingl v)
ctxDel (Here (FixNeg almu) _)  (x `Ac` _)  = applyAlmu almu x
ctxDel (There _ xs) (_ `Ac` xss) = ctxDel xs xss

applyAlmu :: (IsRecEl u, IsRecEl v) => Almu u v -> Usingl u -> Maybe (Usingl v)
applyAlmu (Alspn s) x = applyS applyAtAlmuH (applyAl applyAtAlmuH) s x
applyAlmu (Alins constr ctx) x = inj constr <$> ctxIns ctx x
applyAlmu (Aldel constr ctx) x = case view x of
  (Tag c1 p1) -> do
    Refl <- testEquality constr c1
    ctxDel ctx p1

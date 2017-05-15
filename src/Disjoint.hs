{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Disjoint where

import Data.Type.Equality hiding (apply)

import Multirec
import Lang


disjoint :: Almu u v -> Almu u w -> Bool
disjoint (Alspn s) (Alspn s') = disjointS (disjointAt disjointAlmuH) (disjointAl (disjointAt disjointAlmuH)) s s'
disjoint (Alins constr ctx) (Alins constr' ctx') = disjointCtx disjointAtmuPos ctx ctx'

disjointCtx :: (forall a b . p a -> q b -> Bool)
          -> Ctx p l1 -> Ctx q l2 -> Bool
disjointCtx f (Here p l)    (Here p' l') = f p p'
disjointCtx f (There u ctx) (There u' ctx') = disjointCtx f ctx ctx'
disjointCtx f a (There u ctx) = disjointCtx f a ctx
disjointCtx f (There u ctx) b = disjointCtx f ctx b

disjointAlmuH :: AlmuH u -> AlmuH u -> Bool
disjointAlmuH (AlmuH almu) (AlmuH almu') = disjoint almu almu'

disjointAtmuPos :: AtmuPos u v -> AtmuPos u w -> Bool
disjointAtmuPos (FixPos almu) (FixPos almu') = disjoint almu almu'

disjointS :: (forall a . at1 a -> at2 a -> Bool)
           -> (forall s d . al1 s d -> al2 s d -> Bool)
           -> Spine at1 al1 v
           -> Spine at2 al2 w
           -> Bool
disjointS disAt disAl Scp s'   = True
disjointS disAt disAl s'  Scp  = True
disjointS disAt disAl (Scns c p) (Scns c' p') = case testEquality' c c' of
  Just (Refl, Refl) -> checkAll disAt p p'
  Nothing -> False
 where
   checkAll :: (forall a . at1 a -> at2 a -> Bool)
            -> All at1 s -> All at2 s -> Bool
   checkAll disAt An An = True
   checkAll disAt (a `Ac` as) (b `Ac` bs) = disAt a b && checkAll disAt as bs
disjointS disAt disAl (Schg i j p) (Schg i' j' p') = case testEquality' j j' of
  Just (Refl, Refl) -> case testEquality i i' of
    Just Refl -> disAl p p'
    Nothing -> False
  Nothing -> False

disjointAl :: (forall a . (al1 :: U -> *) a -> (al2 :: U -> *) a -> Bool)
           -> Al al1 s d -> Al al2 s d -> Bool
disjointAl disAt A0 A0 = True
disjointAl disAt (Amod a al) (Amod a' al') = disAt a a' && disjointAl disAt al al'
disjointAl disAt (Ains a al) (Ains a' al') = disjointAl disAt al al'
disjointAl disAt (Adel a al) (Adel a' al') = disjointAl disAt al al'

disjointAt :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
disjointAt disjointR (Ai r) (Ai r') = disjointR r r'
disjointAt disjointR (As p) (As p') = case testEquality new new' of
  Just Refl -> True
  Nothing -> False
  where
    (old, new) = unContract p
    (old', new') = unContract p'

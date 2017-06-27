{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module VCS.Disjoint where

import Data.Type.Equality hiding (apply)

import VCS.Multirec
import Clojure.Lang


disjoint :: Almu u v -> Almu u w -> Bool
-- * Both are spines, easy
disjoint (Alspn s) (Alspn s')
  = disjointS (disjointAt disjointAlmuH)
              (disjointAl (disjointAt disjointAlmuH)) s s'
-- * Insertions
disjoint (Alins constr ctx) almu
  = disjointFromCtxPos disjoint ctx almu
disjoint almu (Alins constr ctx)
  = disjointFromCtxPos disjoint ctx almu
-- * Deletions and copies are trivially disjoint
disjoint (Aldel c ctx) (Alspn Scp)
  = True
disjoint (Alspn Scp) (Aldel c ctx)
  = True
-- * A patch can never be disjoint from itself.
disjoint (Aldel c ctx) (Aldel c' ctx')
  = False
disjoint _ _ = False


-- * Deletions and changes are disjoint, if they align properly.
--   But this is future work
{-
disjoint (Aldel c ctx) (Alspn (Scns c' ats))
  = case testEquality' c c' of
      Just (Refl , Refl) -> _
      Nothing            -> False
-}



disjointFromCtxPos :: (forall a b . Almu u a -> Almu u b -> Bool)
                -> Ctx (AtmuPos u) l1 -> Almu u w -> Bool
disjointFromCtxPos f (Here (FixPos p) l)    almu = f p almu
disjointFromCtxPos f (There u ctx) almu = disjointFromCtxPos f ctx almu

disjointAlmuH :: AlmuH u -> AlmuH u -> Bool
disjointAlmuH (AlmuH almu) (AlmuH almu') = disjoint almu almu'

disjointS :: (forall a . at1 a -> at2 a -> Bool)
           -> (forall s d . al1 s d -> al2 s d -> Bool)
           -> Spine at1 al1 v
           -> Spine at2 al2 w
           -> Bool
disjointS disAt disAl Scp s'   = True
disjointS disAt disAl s'  Scp  = True
disjointS disAt disAl (Scns c p) (Scns c' p')
  = case testEquality' c c' of
          Just (Refl, Refl) -> checkAll disAt p p'
          Nothing -> False
  where
   checkAll :: (forall a . at1 a -> at2 a -> Bool)
            -> All at1 s -> All at2 s -> Bool
   checkAll disAt An An = True
   checkAll disAt (a `Ac` as) (b `Ac` bs) = disAt a b && checkAll disAt as bs
disjointS disAt disAl (Schg i j p) (Schg i' j' p')
  = case testEquality' j j' of
          Just (Refl, Refl) -> case testEquality i i' of
                                  Just Refl -> disAl p p'
                                  Nothing -> False
          Nothing -> False
disjointS disAt disAl _ _ = False


disjointAl :: (forall a . (al1 :: U -> *) a -> (al2 :: U -> *) a -> Bool)
           -> Al al1 s d -> Al al2 s d' -> Bool
disjointAl disAt A0 A0 = True

-- * Insertions
disjointAl disAt al (Ains a' al') = disjointAl disAt al al'
disjointAl disAt (Ains a al) al'  = disjointAl disAt al al'

-- * A patch is disjoint from itself
disjointAl disAt (Adel a al) (Adel a' al')
  = False

-- * Both Amod
disjointAl disAt (Amod a al) (Amod a' al')
  = disAt a a' && disjointAl disAt al al'

-- * Other
disjointAl disAt _ _
  = False

disjointAt :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
disjointAt disjointR (Ai r) (Ai r') = disjointR r r'
disjointAt disjointR (As p) (As p')
  = case testEquality new new' of
          Just Refl -> True
          Nothing -> False
  where
    (old, new) = unContract p
    (old', new') = unContract p'

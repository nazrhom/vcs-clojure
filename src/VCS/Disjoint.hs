{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module VCS.Disjoint where

import Data.Type.Equality hiding (apply)

import VCS.Multirec
import Language.Clojure.Lang
import Language.Common

disjoint :: Almu u v -> Almu u w -> Bool
disjoint =
  disjointAlmu
    (disjointAt (disjointAlmuH disjoint))
    disjoint

structurallyDisjoint :: Almu u v -> Almu u w -> Bool
structurallyDisjoint =
  disjointAlmu
    (structurallyDisjointAt (disjointAlmuH structurallyDisjoint))
    structurallyDisjoint

disjointAlmu :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
             -> (forall u v w . Almu u v -> Almu u w -> Bool)
             -> Almu u v -> Almu u w -> Bool
-- * Both are spines, easy
disjointAlmu compatibleAt _ (Alspn s) (Alspn s')
  = disjointS compatibleAt s s'
-- * Insertions
disjointAlmu _ _ (Alins _ _) (Alins _ _) = False
disjointAlmu _ rec (Alins constr ctx) almu
  = disjointFromCtxPos rec ctx almu
disjointAlmu _ rec almu (Alins constr ctx)
  = disjointFromCtxPos rec ctx almu
-- * Deletions and copies are trivially disjoint
disjointAlmu _ _ (Aldel c ctx) (Alspn Scp)
  = True
disjointAlmu _ _ (Alspn Scp) (Aldel c ctx)
  = True
-- * A patch can never be disjoint from itself.
disjointAlmu _ _ (Aldel c ctx) (Aldel c' ctx')
  = False

-- * Deletions and changes are disjoint, if they align properly.
disjointAlmu _ rec (Aldel c ctx) (Alspn (Scns c' ats))
  = case testEquality' c c' of
      Just (Refl , Refl) -> disjointFromCtxNeg rec ctx ats
      Nothing            -> False
disjointAlmu _ rec (Alspn (Scns c' ats)) (Aldel c ctx)
  = case testEquality' c c' of
      Just (Refl , Refl) -> disjointFromCtxNeg rec ctx ats
      Nothing            -> False
disjointAlmu _ _ _ _ = False


disjointFromCtxNeg :: (forall u v w . Almu u v -> Almu u w -> Bool)
                   -> Ctx (AtmuNeg u) l -> All (At (AlmuH)) l -> Bool
disjointFromCtxNeg rec (Here almu1 rest1) ((Ai almu2) `Ac` rest2)
  = rec (unNeg almu1) (unH almu2) && (checkAll rest2)
  where
    checkAll :: All (At AlmuH) l -> Bool
    checkAll An = True
    checkAll (a `Ac` as) = identityAtAlmu a && checkAll as
disjointFromCtxNeg rec (There a1 ctx) (a2 `Ac` as) = (identityAtAlmu a2) && disjointFromCtxNeg rec ctx as


disjointFromCtxPos :: (forall u v w . Almu u v -> Almu u w -> Bool)
                   -> Ctx (AtmuPos u) l1 -> Almu u w -> Bool
disjointFromCtxPos rec (Here (FixPos p) l)    almu = rec p almu
disjointFromCtxPos rec (There u ctx) almu = disjointFromCtxPos rec ctx almu

disjointAlmuH :: (forall u v w . Almu u v -> Almu u w -> Bool)
               -> AlmuH u -> AlmuH u -> Bool
disjointAlmuH rec (AlmuH almu) (AlmuH almu') = rec almu almu'

disjointS :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
           -> Spine (At AlmuH) (Al (At AlmuH)) u
           -> Spine (At AlmuH) (Al (At AlmuH)) u
           -> Bool
disjointS _ Scp s'
  = True
disjointS _ s'  Scp
  = True
disjointS disjointAt (Scns c p) (Scns c' p')
  = case testEquality c c' of
          Just Refl -> disjAts disjointAt p p'
          Nothing -> False

disjointS disjointAt (Scns c p) (Schg i j p')
 = case testEquality c i of
         Just Refl -> disjAtAl disjointAt p p'
         Nothing   -> False

disjointS disjointAt (Schg i j p') (Scns c p)
 = case testEquality c i of
         Just Refl -> disjAtAl disjointAt p p'
         Nothing   -> False

disjointS _ (Schg i j p) (Schg i' j' p')
  = False

disjAts :: (forall a . at1 a -> at2 a -> Bool)
         -> All at1 s -> All at2 s -> Bool
disjAts disjAt An An = True
disjAts disjAt (a `Ac` as) (b `Ac` bs) = disjAt a b && disjAts disjAt as bs

disjAtAl :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
         -> All (At AlmuH) s -> Al (At AlmuH) s d -> Bool
disjAtAl _ An           _
  = True
disjAtAl disjointAt (a `Ac` as) (Ains at al)
  = disjAtAl disjointAt (a `Ac` as) al
disjAtAl disjointAt (a `Ac` as) (Adel at al)
  = (identityAtAlmu a) && disjAtAl disjointAt as al
disjAtAl disjointAt (a `Ac` as) (Amod at al)
  = disjointAt a at && disjAtAl disjointAt as al

-- disjointAl :: (forall a . (al1 :: U -> *) a -> (al2 :: U -> *) a -> Bool)
--            -> Al al1 s d -> Al al2 s d' -> Bool
-- disjointAl disjAt A0 A0 = True
--
-- -- * Insertions
-- disjointAl disjAt al (Ains a' al') = disjointAl disjAt al al'
-- disjointAl disjAt (Ains a al) al'  = disjointAl disjAt al al'
--
-- -- * A patch is disjoint from itself
-- disjointAl disjAt (Adel a al) (Adel a' al')
--   = False
--
-- -- * Both Amod
-- disjointAl disjAt (Amod a al) (Amod a' al')
--   = disjAt a a' && disjointAl disjAt al al'
--
-- -- * Other
-- disjointAl disjAt _ _
--   = False

disjointAt :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
disjointAt disjointR (Ai r) (Ai r') = disjointR r r'
disjointAt disjointR (As p) (As p')
  = old == new || old' == new'
  where
    (old, new) = unContract p
    (old', new') = unContract p'


structurallyDisjointAt :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
structurallyDisjointAt disjointR (Ai r) (Ai r') = disjointR r r'
structurallyDisjointAt disjointR (As p) (As p')
 = case testEquality new new' of
   Just Refl -> True
   Nothing   -> False
 where
   (old, new) = unContract p
   (old', new') = unContract p'

---- Identity ------
identityAtAlmu :: At AlmuH v -> Bool
identityAtAlmu = identityAt (identityAlmu . unH)

identityAlmu :: Almu u v -> Bool
identityAlmu (Alspn Scp) = True
identityAlmu _         = False

identityAt :: (IsRecEl a => rec a -> Bool)
              -> At rec a -> Bool
identityAt idRec (Ai r) = idRec r
identityAt idRec (As c) = old == new
  where
    (old, new) = unContract c

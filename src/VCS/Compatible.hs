{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module VCS.Compatible where

import Data.Type.Equality hiding (apply)

import VCS.Multirec
import VCS.Disjoint
import Language.Clojure.Lang
import Language.Common


compatible :: Almu u v -> Almu u w -> Bool
compatible =
  compatibleAlmu
    (compatibleAt (compatibleAlmuH compatible))
    compatible

structurallyCompatible :: Almu u v -> Almu u w -> Bool
structurallyCompatible =
  compatibleAlmu
    (structurallyCompatibleAt (compatibleAlmuH structurallyCompatible))
    structurallyCompatible
compatibleAlmu :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
           -> (forall u v w . Almu u v -> Almu u w -> Bool)
           -> Almu u v -> Almu u w -> Bool
-- * Both are spines, easy
compatibleAlmu compatibleAt _ (Alspn s) (Alspn s')
  = compatibleS compatibleAt s s'
-- * Insertions
compatibleAlmu compatibleAt rec (Alins c ctx) (Alins c' ctx')
  = case testEquality' c c' of
    Just (Refl, Refl) -> matchCtxPos rec ctx ctx'
    Nothing -> False
compatibleAlmu _ rec (Alins constr ctx) almu
  = compatibleFromCtxPos rec ctx almu
compatibleAlmu _ rec almu (Alins constr ctx)
  = compatibleFromCtxPos rec ctx almu
-- * Deletions and copies are trivially compatible
compatibleAlmu _ _ (Aldel c ctx) (Alspn Scp)
  = True
compatibleAlmu _ _ (Alspn Scp) (Aldel c ctx)
  = True

compatibleAlmu _ rec (Aldel c ctx) (Aldel c' ctx')
  = case testEquality' c c' of
    Just (Refl, Refl) -> matchCtxNeg rec ctx ctx'
    Nothing -> False

-- * Deletions and changes are compatible, if they align properly.
compatibleAlmu _ rec (Aldel c ctx) (Alspn (Scns c' ats))
  = case testEquality' c c' of
      Just (Refl , Refl) -> compatibleFromCtxNeg rec ctx ats
      Nothing            -> False
compatibleAlmu _ rec (Alspn (Scns c' ats)) (Aldel c ctx)
  = case testEquality' c c' of
      Just (Refl , Refl) -> compatibleFromCtxNeg rec ctx ats
      Nothing            -> False
compatibleAlmu _ _ _ _ = False

compatibleAlmuH :: (forall u v w . Almu u v -> Almu u w -> Bool)
                -> AlmuH u -> AlmuH u -> Bool
compatibleAlmuH rec (AlmuH almu) (AlmuH almu') = rec almu almu'


compatibleFromCtxNeg :: (forall u v w . Almu u v -> Almu u w -> Bool)
                     -> Ctx (AtmuNeg u) l -> All (At (AlmuH)) l -> Bool
compatibleFromCtxNeg rec (Here almu1 rest1) ((Ai almu2) `Ac` rest2)
  = rec (unNeg almu1) (unH almu2) && (checkAll rest2)
  where
    checkAll :: All (At AlmuH) l -> Bool
    checkAll An = True
    checkAll (a `Ac` as) = identityAtAlmu a && checkAll as
compatibleFromCtxNeg rec (There a1 ctx) (a2 `Ac` as) =
  identityAtAlmu a2 && compatibleFromCtxNeg rec ctx as

compatibleFromCtxPos :: (forall u v w . Almu u v -> Almu u w -> Bool)
                     -> Ctx (AtmuPos u) l1 -> Almu u w -> Bool
compatibleFromCtxPos rec (Here (FixPos p) l) almu
  = rec p almu
compatibleFromCtxPos rec (There u ctx) almu
  = compatibleFromCtxPos rec ctx almu

matchCtxPos :: (forall u v w . Almu u v -> Almu u w -> Bool)
            -> Ctx (AtmuPos u) l -> Ctx (AtmuPos u) l -> Bool
matchCtxPos rec (Here (FixPos almu1) rest1) (Here (FixPos almu2) rest2)
  = rec almu1 almu2 && checkAll rest1 rest2
matchCtxPos rec (There a1 ctx1)             (There a2 ctx2)
  = a1 == a2 && matchCtxPos rec ctx1 ctx2

matchCtxNeg :: (forall u v w . Almu u v -> Almu u w -> Bool)
            -> Ctx (AtmuNeg v) l -> Ctx (AtmuNeg w) l -> Bool
matchCtxNeg rec (Here (FixNeg almu1) rest1) (Here (FixNeg almu2) rest2)
  = rec almu1 almu2 && checkAll rest1 rest2
matchCtxNeg rec (There a1 ctx1)             (There a2 ctx2)
  = a1 == a2 && matchCtxNeg rec ctx1 ctx2

checkAll :: All Usingl l -> All Usingl l -> Bool
checkAll An            An            = True
checkAll (a1 `Ac` as1) (a2 `Ac` as2) = a1 == a2 && checkAll as1 as2

compatibleS :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
           -> Spine (At AlmuH) (Al (At AlmuH)) u
           -> Spine (At AlmuH) (Al (At AlmuH)) u
           -> Bool
compatibleS _ Scp s'
  = True
compatibleS _ s'  Scp
  = True
compatibleS compatibleAt (Scns c p) (Scns c' p')
  = case testEquality c c' of
          Just Refl -> compatibleAts compatibleAt p p'
          Nothing -> False

compatibleS compatibleAt (Scns c p) (Schg i j p')
 = case testEquality c i of
         Just Refl -> compatibleAtAl compatibleAt p p'
         Nothing   -> False

compatibleS compatibleAt (Schg i j p') (Scns c p)
 = case testEquality c i of
         Just Refl -> compatibleAtAl compatibleAt p p'
         Nothing   -> False

compatibleS compatibleAt (Schg i j p) (Schg i' j' p')
  = case testEquality' j j' of
    Just (Refl, Refl) -> case testEquality' i i' of
      Just (Refl,Refl) -> compatibleAl compatibleAt p p'
      Nothing -> False
    Nothing -> False

compatibleAts :: (forall a . at1 a -> at2 a -> Bool)
         -> All at1 s -> All at2 s -> Bool
compatibleAts compatibleAt An An = True
compatibleAts compatibleAt (a `Ac` as) (b `Ac` bs) = compatibleAt a b && compatibleAts compatibleAt as bs


compatibleAtAl :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
                -> All (At AlmuH) s -> Al (At AlmuH) s d -> Bool
compatibleAtAl _ An           _
  = True
compatibleAtAl compatibleAt (a `Ac` as) (Ains at al)
  = compatibleAtAl compatibleAt (a `Ac` as) al
compatibleAtAl compatibleAt (a `Ac` as) (Adel at al)
  = (identityAtAlmu a) && compatibleAtAl compatibleAt as al
compatibleAtAl compatibleAt (a `Ac` as) (Amod at al)
  = compatibleAt a at && compatibleAtAl compatibleAt as al

compatibleAl :: (forall a . (At AlmuH) a -> (At AlmuH) a -> Bool)
              -> Al (At AlmuH) s d -> Al (At AlmuH) s d' -> Bool
compatibleAl _ A0 A0 = True

-- * Insertions
compatibleAl compatibleAt (Ains a al) (Ains a' al')
  = case testEquality a a' of
    Just Refl -> a == a' && compatibleAl compatibleAt al al'
    Nothing   -> False
compatibleAl compatibleAt al (Ains a' al')
  = compatibleAl compatibleAt al al'
compatibleAl compatibleAt (Ains a al) al'
  = compatibleAl compatibleAt al al'

compatibleAl compatibleAt (Adel a al) (Adel a' al')
  = case testEquality a a' of
    Just Refl -> a == a' && compatibleAl compatibleAt al al'
    Nothing   -> False

-- * Mod and deletion

compatibleAl compatibleAt (Adel a al) (Amod a' al')
  = identityAtAlmu a' && compatibleAl compatibleAt al al'

compatibleAl compatibleAt (Amod a al) (Adel a' al')
  = identityAtAlmu a && compatibleAl compatibleAt  al al'

compatibleAl compatibleAt (Amod a al) (Amod a' al')
  = compatibleAt a a' && compatibleAl compatibleAt al al'


compatibleAt :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
compatibleAt compatibleR (Ai r) (Ai r') = compatibleR r r'
compatibleAt compatibleR (As p) (As p')
  = old == new || old' == new' || new == new'
  where
    (old, new) = unContract p
    (old', new') = unContract p'

structurallyCompatibleAt  :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
structurallyCompatibleAt = structurallyDisjointAt

-- ---- Identity ------
-- identityAtAlmu :: At AlmuH v -> Bool
-- identityAtAlmu = identityAt (identityAlmu . unH)
--
-- identityAlmu :: Almu u v -> Bool
-- identityAlmu (Alspn Scp) = True
-- identityAlmu _         = False
--
-- identityAt :: (IsRecEl a => rec a -> Bool)
--               -> At rec a -> Bool
-- identityAt idRec (Ai r) = idRec r
-- identityAt idRec (As c) = old == new
--   where
--     (old, new) = unContract c

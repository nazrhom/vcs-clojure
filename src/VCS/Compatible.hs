{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module VCS.Compatible where

import Data.Type.Equality hiding (apply)

import VCS.Multirec
import Clojure.Lang


compatible :: Almu u v -> Almu u w -> Bool
-- * Both are spines, easy
compatible (Alspn s) (Alspn s')
  = compatibleS s s'
-- * Insertions
compatible (Alins c ctx) (Alins c' ctx')
  = case testEquality' c c' of
    Just (Refl, Refl) -> matchCtxPos ctx ctx'
    Nothing -> False
compatible (Alins constr ctx) almu
  = compatibleFromCtxPos ctx almu
compatible almu (Alins constr ctx)
  = compatibleFromCtxPos ctx almu
-- * Deletions and copies are trivially compatible
compatible (Aldel c ctx) (Alspn Scp)
  = True
compatible (Alspn Scp) (Aldel c ctx)
  = True

compatible (Aldel c ctx) (Aldel c' ctx')
  = case testEquality' c c' of
    Just (Refl, Refl) -> matchCtxNeg ctx ctx'
    Nothing -> False

-- * Deletions and changes are compatible, if they align properly.
compatible (Aldel c ctx) (Alspn (Scns c' ats))
  = case testEquality' c c' of
      Just (Refl , Refl) -> compatibleFromCtxNeg ctx ats
      Nothing            -> False
compatible (Alspn (Scns c' ats)) (Aldel c ctx)
  = case testEquality' c c' of
      Just (Refl , Refl) -> compatibleFromCtxNeg ctx ats
      Nothing            -> False
compatible _ _ = False

compatibleAlmuH :: AlmuH u -> AlmuH u -> Bool
compatibleAlmuH (AlmuH almu) (AlmuH almu') = compatible almu almu'

compatibleFromCtxNeg :: Ctx (AtmuNeg u) l -> All (At (AlmuH)) l -> Bool
compatibleFromCtxNeg (Here almu1 rest1) ((Ai almu2) `Ac` rest2) = compatible (unNeg almu1) (unH almu2) && (checkAll rest2)
  where
    checkAll :: All (At AlmuH) l -> Bool
    checkAll An = True
    checkAll (a `Ac` as) = identityAtAlmu a && checkAll as
compatibleFromCtxNeg (There a1 ctx) (a2 `Ac` as) = (identityAtAlmu a2) && compatibleFromCtxNeg ctx as

compatibleFromCtxPos :: Ctx (AtmuPos u) l1 -> Almu u w -> Bool
compatibleFromCtxPos (Here (FixPos p) l)    almu = compatible p almu
compatibleFromCtxPos (There u ctx) almu = compatibleFromCtxPos ctx almu

matchCtxPos :: Ctx (AtmuPos u) l -> Ctx (AtmuPos u) l -> Bool
matchCtxPos (Here (FixPos almu1) rest1) (Here (FixPos almu2) rest2) = compatible almu1 almu2 && checkAll rest1 rest2
  where
    checkAll :: All Usingl l -> All Usingl l -> Bool
    checkAll An            An            = True
    checkAll (a1 `Ac` as1) (a2 `Ac` as2) = a1 == a2 && checkAll as1 as2
matchCtxPos (There a1 ctx1)    (There a2 ctx2)   = a1 == a2 && matchCtxPos ctx1 ctx2

matchCtxNeg :: Ctx (AtmuNeg v) l -> Ctx (AtmuNeg w) l -> Bool
matchCtxNeg (Here (FixNeg almu1) rest1) (Here (FixNeg almu2) rest2) = compatible almu1 almu2
matchCtxNeg (There a1 ctx1)    (There a2 ctx2)   = matchCtxNeg ctx1 ctx2

compatibleS ::  Spine (At AlmuH) (Al (At AlmuH)) u
           -> Spine (At AlmuH) (Al (At AlmuH)) u
           -> Bool
compatibleS Scp s'
  = True
compatibleS s'  Scp
  = True
compatibleS (Scns c p) (Scns c' p')
  = case testEquality c c' of
          Just Refl -> compatibleAts (compatibleAt compatibleAlmuH) p p'
          Nothing -> False

compatibleS (Scns c p) (Schg i j p')
 = case testEquality c i of
         Just Refl -> compatibleAtAl p p'
         Nothing   -> False

compatibleS (Schg i j p') (Scns c p)
 = case testEquality c i of
         Just Refl -> compatibleAtAl p p'
         Nothing   -> False

compatibleS (Schg i j p) (Schg i' j' p')
  = case testEquality' j j' of
    Just (Refl, Refl) -> case testEquality' i i' of
      Just (Refl,Refl) -> compatibleAl p p'
      Nothing -> False
    Nothing -> False

compatibleAts :: (forall a . at1 a -> at2 a -> Bool)
         -> All at1 s -> All at2 s -> Bool
compatibleAts compatibleAt An An = True
compatibleAts compatibleAt (a `Ac` as) (b `Ac` bs) = compatibleAt a b && compatibleAts compatibleAt as bs

compatibleAtAl :: All (At AlmuH) s -> Al (At AlmuH) s d -> Bool
compatibleAtAl An           _
  = True
compatibleAtAl (a `Ac` as) (Ains at al)
  = compatibleAtAl (a `Ac` as) al
compatibleAtAl (a `Ac` as) (Adel at al)
  = (identityAtAlmu a) && compatibleAtAl as al
compatibleAtAl (a `Ac` as) (Amod at al)
  = (compatibleAt compatibleAlmuH a at) && compatibleAtAl as al

compatibleAl :: Al (At AlmuH) s d -> Al (At AlmuH) s d' -> Bool
compatibleAl A0 A0 = True

-- * Insertions
compatibleAl (Ains a al) (Ains a' al')
  = case testEquality a a' of
    Just Refl -> a == a' && compatibleAl al al'
    Nothing   -> False
compatibleAl al (Ains a' al')
  = compatibleAl al al'
compatibleAl (Ains a al) al'
  = compatibleAl al al'

compatibleAl (Adel a al) (Adel a' al')
  = compatibleAl al al'

-- * Mod and deletion

compatibleAl (Adel a al) (Amod a' al')
  = identityAtAlmu a' && compatibleAl al al'

compatibleAl (Amod a al) (Adel a' al')
  = identityAtAlmu a && compatibleAl al al'

compatibleAl (Amod a al) (Amod a' al')
  = (compatibleAt compatibleAlmuH) a a' && compatibleAl al al'


compatibleAt :: (IsRecEl a => rec1 a -> rec2 a -> Bool)
           -> At rec1 a -> At rec2 a -> Bool
compatibleAt compatibleR (Ai r) (Ai r') = compatibleR r r'
compatibleAt compatibleR (As p) (As p')
  = new == new'
    -- case testEquality new new' of
    -- Just Refl -> True
    -- Nothing   -> False
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

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
-- * Both are spines, easy
disjoint (Alspn s) (Alspn s')
  = disjointS s s'
-- * Insertions
disjoint (Alins _ _) (Alins _ _) = False
disjoint (Alins constr ctx) almu
  = disjointFromCtxPos ctx almu
disjoint almu (Alins constr ctx)
  = disjointFromCtxPos ctx almu
-- * Deletions and copies are trivially disjoint
disjoint (Aldel c ctx) (Alspn Scp)
  = True
disjoint (Alspn Scp) (Aldel c ctx)
  = True
-- * A patch can never be disjoint from itself.
disjoint (Aldel c ctx) (Aldel c' ctx')
  = False

-- * Deletions and changes are disjoint, if they align properly.
disjoint (Aldel c ctx) (Alspn (Scns c' ats))
  = case testEquality' c c' of
      Just (Refl , Refl) -> disjointFromCtxNeg ctx ats
      Nothing            -> False
disjoint (Alspn (Scns c' ats)) (Aldel c ctx)
  = case testEquality' c c' of
      Just (Refl , Refl) -> disjointFromCtxNeg ctx ats
      Nothing            -> False
disjoint _ _ = False


disjointFromCtxNeg :: Ctx (AtmuNeg u) l -> All (At (AlmuH)) l -> Bool
disjointFromCtxNeg (Here almu1 rest1) ((Ai almu2) `Ac` rest2) = disjoint (unNeg almu1) (unH almu2) && (checkAll rest2)
  where
    checkAll :: All (At AlmuH) l -> Bool
    checkAll An = True
    checkAll (a `Ac` as) = identitiyAtAlmu a && checkAll as
disjointFromCtxNeg (There a1 ctx) (a2 `Ac` as) = (identitiyAtAlmu a2) && disjointFromCtxNeg ctx as


disjointFromCtxPos :: Ctx (AtmuPos u) l1 -> Almu u w -> Bool
disjointFromCtxPos (Here (FixPos p) l)    almu = disjoint p almu
disjointFromCtxPos (There u ctx) almu = disjointFromCtxPos ctx almu

disjointAlmuH :: AlmuH u -> AlmuH u -> Bool
disjointAlmuH (AlmuH almu) (AlmuH almu') = disjoint almu almu'

disjointS ::  Spine (At AlmuH) (Al (At AlmuH)) u
           -> Spine (At AlmuH) (Al (At AlmuH)) u
           -> Bool
disjointS Scp s'
  = True
disjointS s'  Scp
  = True
disjointS (Scns c p) (Scns c' p')
  = case testEquality c c' of
          Just Refl -> disjAts (disjointAt disjointAlmuH) p p'
          Nothing -> False

disjointS (Scns c p) (Schg i j p')
 = case testEquality c i of
         Just Refl -> disjAtAl p p'
         Nothing   -> False

disjointS (Schg i j p') (Scns c p)
 = case testEquality c i of
         Just Refl -> disjAtAl p p'
         Nothing   -> False

disjointS (Schg i j p) (Schg i' j' p')
  = False

disjAts :: (forall a . at1 a -> at2 a -> Bool)
         -> All at1 s -> All at2 s -> Bool
disjAts disjAt An An = True
disjAts disjAt (a `Ac` as) (b `Ac` bs) = disjAt a b && disjAts disjAt as bs

disjAtAl :: All (At AlmuH) s -> Al (At AlmuH) s d -> Bool
disjAtAl An           _
  = True
disjAtAl (a `Ac` as) (Ains at al)
  = disjAtAl (a `Ac` as) al
disjAtAl (a `Ac` as) (Adel at al)
  = (identitiyAtAlmu a) && disjAtAl as al
disjAtAl (a `Ac` as) (Amod at al)
  = (disjointAt disjointAlmuH a at) && disjAtAl as al

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
  = new == new'
    -- case testEquality new new' of
    -- Just Refl -> True
    -- Nothing   -> False
  where
    (old, new) = unContract p
    (old', new') = unContract p'



---- Identity ------
identitiyAtAlmu :: At AlmuH v -> Bool
identitiyAtAlmu = identityAt (identityAlmu . unH)

identityAlmu :: Almu u v -> Bool
identityAlmu (Alspn Scp) = True
identityAlmu _         = False

identityAt :: (IsRecEl a => rec a -> Bool)
              -> At rec a -> Bool
identityAt idRec (Ai r) = idRec r
identityAt idRec (As c) = old == new
  where
    (old, new) = unContract c

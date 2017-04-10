{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Multirec where

import Data.Kind
import Data.Type.Equality hiding (apply)
import Control.Applicative
import Lang

import Parser

-- The actual puzzle

data Spine (at :: U -> *)(al :: [U] -> [U] -> *) :: U -> * where
  Scp  :: Spine at al u
  Scns :: ConstrFor u s -> All at (TypeOf s) -> Spine at al u
  Schg :: ConstrFor u s -> ConstrFor u r
       -> al (TypeOf s) (TypeOf r)
       -> Spine at al u

data Al (at :: U -> *) :: [U] -> [U] -> * where
  A0   :: Al at '[] '[]
  Ains :: Usingl u -> Al at xs ys -> Al at xs (u ': ys)
  Adel :: Usingl u -> Al at xs ys -> Al at (u ': xs) ys
  Amod :: at u -> Al at xs ys -> Al at (u ': xs) (u ': ys)

showAl :: Al at p1 p2 -> String
showAl A0 = "0"
showAl (Ains _ al) = '+' : showAl al
showAl (Adel _ al) = '-' : showAl al
showAl (Amod _ al) = '%' : showAl al



data At (recP :: U -> *) :: U -> * where
  Ai :: (IsRecEl u) => recP u -> At recP u
  As :: Trivial Usingl u -> At recP u

data Almu :: U -> U -> * where
  Alspn :: Spine (At AlmuH) (Al (At AlmuH)) u -> Almu u u
  Alins :: ConstrFor v s -> Ctx (AtmuPos u) (TypeOf s) -> Almu u v
  Aldel :: ConstrFor u s -> Ctx (AtmuNeg v) (TypeOf s) -> Almu u v

data AlmuH :: U -> * where
  AlmuH :: Almu u u -> AlmuH u

-- Atmu positive and negative variations
data AtmuPos (v :: U) :: U -> * where
  FixPos :: Almu v u -> AtmuPos v u

data AtmuNeg (v :: U) :: U -> * where
  FixNeg :: Almu u v -> AtmuNeg v u

data Ctx (r :: U -> *) :: [U] -> * where
  Here :: (IsRecEl u) => r u       -> All Usingl l  -> Ctx r (u ': l)
  There :: Usingl u -> Ctx r l       -> Ctx r (u ': l)

spine :: IsRecEl r => Usingl r -> Usingl r -> Spine TrivialA TrivialP r
spine x y | x == y = Scp
spine x y | otherwise = case (view x, view y) of
  ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
    Just Refl -> Scns c1 (zipP l1 l2)
    Nothing -> Schg c1 c2 (Pair l1 l2)

-- |Implement DP-style optimization for Alignments and Recursive alignmetns.
--
-- The phase records the LAST decision took by the algo (either an insertion,
-- modification ordeletion)
data Phase
  = I | M | D
  deriving (Eq , Show)

align :: (Alternative m) => All Usingl p1 -> All Usingl p2 -> m (Al TrivialA p1 p2)
align = alignOpt M

shouldAlign :: (Alternative m)
            => Usingl a1 -> Usingl a2 -> m (Al TrivialA p1 p2)
            -> m (Al TrivialA (a1 ': p1) (a2 ': p2))
shouldAlign at1 at2 rest
  = case testEquality at1 at2 of
      Just Refl -> Amod (Contract (at1, at2)) <$> rest
      Nothing   -> empty

alignOpt :: (Alternative m) => Phase -> All Usingl p1 -> All Usingl p2 -> m (Al TrivialA p1 p2)
alignOpt _ An           An          = pure A0

-- alignOp D == align-no-ins
alignOpt D An           (b `Ac` pb) = Ains b <$> alignOpt I An pb
alignOpt D (a `Ac` pa)  An          = empty
alignOpt D (a `Ac` pa)  (b `Ac` pb) = shouldAlign a b (align pa pb)
                                  <|> Adel a <$> alignOpt D pa (b .@. pb)

-- alignOpt I == align-no-del
alignOpt I An           (b `Ac` pb) = Ains b <$> alignOpt I An pb
alignOpt I (a `Ac` pa)  An          = empty
alignOpt I (a `Ac` pa)  (b `Ac` pb) = shouldAlign a b (align pa pb)
                                  <|> Ains b <$> alignOpt I (a .@. pa) pb

-- alignOpt M == align*
alignOpt M An           (b `Ac` pb) = Ains b <$> alignOpt I An pb
alignOpt M (a `Ac` pa)  An          = Adel a <$> alignOpt D pa An
alignOpt M (a `Ac` pa)  (b `Ac` pb) = shouldAlign a b (align pa pb)
                                  <|> Ains b <$> alignOpt I (a .@. pa) pb
                                  <|> Adel a <$> alignOpt D pa (b .@. pb)

{-
e1 = Comment "la"
e2 = Comment "l"
e3 = Comment "asdsdj3"
s1 = Space
s2 = Comma
l1 = Nil

test :: [Al TrivialA '[KExpr , KExpr] '[KExpr , KExpr]]
test = align (UExpr e3 .@. (UExpr e3 .@. An))
             (UExpr e1 .@. (UExpr e2 .@. An))
-}

-- Library stuff
newtype Contract (f :: k -> *) (x :: k) = Contract { unContract :: (f x , f x) }


type Trivial = Contract
type TrivialA = Contract Usingl
data TrivialP :: [U] -> [U] -> * where
 Pair :: All Usingl l -> All Usingl r -> TrivialP l r

mapAll :: (forall a . p a -> q a) -> All p l -> All q l
mapAll f An = An
mapAll f (a `Ac` as) = f a `Ac` mapAll f as

mapAllM :: Monad m => (forall a . p a -> m (q a))
        ->  All p xs -> m (All q xs)
mapAllM f An = return An
mapAllM f (px `Ac` pxs) = Ac <$> f px <*> mapAllM f pxs

zipP :: All p a -> All p a -> All (Contract p) a
zipP An          An          = An
zipP (a `Ac` as) (b `Ac` bs) = Contract (a, b) .@. zipP as bs

mapSpineM :: Monad m => (forall a . at1 a -> m (at2 a))
     -> (forall s d . al1 s d -> m (al2 s d))
     -> Spine at1 al1 u -> m (Spine at2 al2 u)
mapSpineM f g Scp             = return Scp
mapSpineM f g (Scns c ps)     = Scns c <$> mapAllM f ps
mapSpineM f g (Schg c1 c2 al) = Schg c1 c2 <$> g al

mapAlM :: Monad m => (forall a . at1 a -> m (at2 a))
          -> Al at1 s d -> m (Al at2 s d)
mapAlM f A0           = return A0
mapAlM f (Adel at al) = Adel at <$> mapAlM f al
mapAlM f (Ains at al) = Ains at <$> mapAlM f al
mapAlM f (Amod at al) = Amod    <$> f at <*> mapAlM f al

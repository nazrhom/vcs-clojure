{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Multirec where

import Data.Kind
import Data.Type.Equality hiding (apply)
import Control.Applicative
import qualified Data.Map  as M
import Control.Monad.Reader

import Debug.Trace

import Parser
import Lang
import Oracle

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

data At (recP :: U -> *) :: U -> * where
  Ai :: (IsRecEl u) => recP u -> At recP u
  As :: TrivialA u -> At recP u

data Almu :: U -> U -> * where
  Alspn :: Spine (At AlmuH) (Al (At AlmuH)) u -> Almu u u
  Alins :: ConstrFor v s -> Ctx (AtmuPos u) (TypeOf s) -> Almu u v
  Aldel :: ConstrFor u s -> Ctx (AtmuNeg v) (TypeOf s) -> Almu u v

data AlmuH :: U -> * where
  AlmuH :: Almu u u -> AlmuH u
  deriving Show

unH :: AlmuH u -> Almu u u
unH (AlmuH u) = u

-- Atmu positive and negative variations
data AtmuPos (v :: U) :: U -> * where
  FixPos :: Almu v u -> AtmuPos v u
  deriving Show

unPos :: AtmuPos v u -> Almu v u
unPos (FixPos p) = p

data AtmuNeg (v :: U) :: U -> * where
  FixNeg :: Almu u v -> AtmuNeg v u
  deriving Show

unNeg :: AtmuNeg u v -> Almu v u
unNeg (FixNeg n) = n

data Ctx (r :: U -> *) :: [U] -> * where
  Here :: (IsRecEl u) => r u -> All Usingl l  -> Ctx r (u ': l)
  There :: Usingl u -> Ctx r l -> Ctx r (u ': l)

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

align :: (MonadOracle o m)
      => o -> All Usingl p1 -> All Usingl p2
      -> m (Al TrivialA p1 p2)
align orc p1 p2 = runReaderT (alignO orc p1 p2) []

alignO :: (MonadOracle o m)
       => o -> All Usingl p1 -> All Usingl p2
       -> HistoryM m (Al TrivialA p1 p2)
alignO orc An An = return A0
alignO orc p1 p2
  = do
    paths <- callP orc p1 p2
    followAllPaths paths orc p1 p2

followAllPaths :: (MonadOracle o m)
               => [Path] -> o -> All Usingl p1 -> All Usingl p2
               -> HistoryM m (Al TrivialA p1 p2)
followAllPaths []     _   _  _
  = empty
followAllPaths (i:is) orc p1 p2
  = (followPath i orc p1 p2) <|> (followAllPaths is orc p1 p2)


-- * Follows one specific path. Makes sure the recursive call to
--   alignO has access to this path, for later inspection.
followPath :: (MonadOracle o m)
           => Path -> o -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
followPath _ orc An An         = pure A0 -- XXX: Should never be called!
followPath I orc p1 (a `Ac` p) = Ains a <$> local (I:) (alignO orc p1 p)
followPath D orc (a `Ac` p) p2 = Adel a <$> local (D:) (alignO orc p p2)
followPath M orc (a1 `Ac` p1) (a2 `Ac` p2)
  = case testEquality a1 a2 of
      Just Refl -> Amod (Contract (a1 , a2)) <$> local (M:) (alignO orc p1 p2)
      Nothing   -> empty
followPath p orc p1 p2 = trace
    (  "Phase: " ++ show p
    ++ "\np1: "  ++ show p1
    ++ "\np2: "  ++ show p2) undefined

-- Library stuff
newtype Contract (f :: k -> *) (x :: k) = Contract { unContract :: (f x , f x) }

type TrivialA = Contract Usingl
data TrivialP :: [U] -> [U] -> * where
 Pair :: All Usingl l -> All Usingl r -> TrivialP l r

mapAll :: (forall a . p a -> q a) -> All p l -> All q l
mapAll f An = An
mapAll f (a `Ac` as) = f a `Ac` mapAll f as

foldAll :: (forall a . p a -> b -> b) -> b -> All p a -> b
foldAll f b An          = b
foldAll f b (a `Ac` as) = f a (foldAll f b as)

foldCtx :: (forall u . r u -> b -> b)
        -> (forall u . Usingl u -> b -> b)
        -> b -> Ctx r l -> b
foldCtx f g b (Here r p)  = f r (foldAll g b p)
foldCtx f g b (There u c) = foldCtx f g b c

mapAllM :: Monad m => (forall a . p a -> m (q a))
        -> All p xs -> m (All q xs)
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

mapAt :: (forall a . rec1 a -> rec2 a)
      -> At rec1 a -> At rec2 a
mapAt f (Ai r) = Ai (f r)
mapAt f (As t) = As t
-- Show instances
instance Show (Almu u v) where
  show = showAlmu
instance Show (Al at p1 p2) where
  show = showAl
instance Show (At AlmuH u) where
  show = showAt
instance Show (Spine (At AlmuH) al u) where
  show = showSpine
instance Show (All Usingl l) where
  show An = ""
  show (a `Ac` as) = show a ++ " " ++ show as
instance Show (All (At AlmuH) l) where
  show = showAll
instance Show (Ctx (AtmuPos u) p) where
  show = showCtxP
instance Show (Ctx (AtmuNeg u) p) where
  show = showCtxN
instance Show (f x) => Show (Contract f x) where
  show c = show $ unContract c

showAll :: All (At AlmuH) l -> String
showAll An = ""
showAll (x `Ac` xs) = show x ++ show xs

showSpine :: Spine (At AlmuH) al u -> String
showSpine (Scp) = "Scp. "
showSpine (Scns i p) = "Scns " ++ show i ++ " (" ++ show p ++ ") "
showSpine (Schg i j p) = "Schg From: " ++ show i ++ " to " ++ show j ++ "{missing almu}"

showAlmu :: Almu u v -> String
showAlmu (Alspn s) = "M-" ++ show s
showAlmu (Alins c d) = "I-" ++ show d
showAlmu (Aldel c d) = "D-" ++ show d

showCtxP :: Ctx (AtmuPos u) p -> String
showCtxP (Here r p) = show r
showCtxP (There u c) = show c

showCtxN :: Ctx (AtmuNeg u) p -> String
showCtxN (Here r p) = show r
showCtxN (There u c) = show c

showAt :: At AlmuH u -> String
showAt (Ai r) = show r
showAt (As t) = show t

showAl :: Al at p1 p2 -> String
showAl A0 = "0"
showAl (Ains _ al) = '+' : showAl al
showAl (Adel _ al) = '-' : showAl al
showAl (Amod _ al) = '%' : showAl al

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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}


module VCS.Multirec where

import Data.Kind
import Data.Type.Equality hiding (apply)
import Control.Applicative
import qualified Data.Map  as M
import Control.Monad.Reader
import Unsafe.Coerce
import GHC.Generics

import Debug.Trace

import Clojure.AST
import Clojure.Lang
import Oracle.Oracle

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
  deriving (Show, Generic)

unH :: AlmuH u -> Almu u u
unH (AlmuH u) = u

-- Atmu positive and negative variations
data AtmuPos (v :: U) :: U -> * where
  FixPos :: Almu v u -> AtmuPos v u
  deriving (Show, Generic)

unPos :: AtmuPos v u -> Almu v u
unPos (FixPos p) = p

data AtmuNeg (v :: U) :: U -> * where
  FixNeg :: Almu u v -> AtmuNeg v u
  deriving (Show, Generic)

unNeg :: AtmuNeg u v -> Almu v u
unNeg (FixNeg n) = n

data Ctx (r :: U -> *) :: [U] -> * where
  Here :: (IsRecEl u) => r u -> All Usingl l  -> Ctx r (u ': l)
  There :: Usingl u -> Ctx r l -> Ctx r (u ': l)

-- |Implement DP-style optimization for Alignments and Recursive alignmetns.
--
-- The phase records the LAST decision took by the algo (either an insertion,
-- modification ordeletion)
align :: (MonadOracle o m)
      => o -> Int -> Int -> All Usingl p1 -> All Usingl p2
      -> m (Al TrivialA p1 p2)
align orc maxCost currCost p1 p2 = runReaderT (alignO orc maxCost p1 p2) (History { path = [I,M,D], cost = currCost })

alignO :: (MonadOracle o m)
       => o -> Int -> All Usingl p1 -> All Usingl p2
       -> HistoryM m (Al TrivialA p1 p2)
alignO orc maxCost An An = return A0
alignO orc maxCost p1 p2 = do
  paths <- callP orc p1 p2
  followAllPaths paths orc maxCost p1 p2

followAllPaths :: (MonadOracle o m)
               => [Path] -> o -> Int -> All Usingl p1 -> All Usingl p2
               -> HistoryM m (Al TrivialA p1 p2)
followAllPaths []     _   _   _  _
  = empty
followAllPaths (i:is) orc maxCost p1 p2
  = (followPath i orc maxCost p1 p2) <|> (followAllPaths is orc maxCost p1 p2)


-- * Follows one specific path. Makes sure the recursive call to
--   alignO has access to this path, for later inspection.
followPath :: (MonadOracle o m)
           => Path -> o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
followPath _ orc maxCost An An         = pure A0
followPath I orc maxCost x y = alignIns orc maxCost x y
followPath D orc maxCost x y = alignDel orc maxCost x y
followPath M orc maxCost x y = alignMod orc maxCost x y
-- followPath FM orc (a1 `Ac` p1) (a2 `Ac` p2)
--   = case testEquality a1 a2 of
--       Just Refl -> Amod (Contract (a1 , a2)) <$> local (M:) (alignO orc p1 p2)
--       Nothing -> case unsafeCoerceEquality a1 a2 of
--           Just Refl -> do
--             Amod (Contract (a1 , a2)) <$> local (M:) (alignO orc p1 p2)
-- followPath S orc x y = alignAll NoDupBranches x y

liftPath :: ([Path] -> [Path]) -> History -> History
liftPath f = liftHistory (f,id)

liftCost :: (Int -> Int) -> History -> History
liftCost f = liftHistory (id,f)

liftHistory :: (([Path] -> [Path], Int -> Int)) -> History -> History
liftHistory (fp, fc) h
  = History {
      path = fp (path h)
    , cost = fc (cost h)
    }

alignIns :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignIns orc maxCost p1 (a `Ac` p) =
  Ains a <$> local (liftPath (I:)) (alignO orc maxCost p1 p)

alignDel :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignDel orc maxCost (a `Ac` p) p2 =
  Adel a <$> local (liftPath (D:)) (alignO orc maxCost p p2)

alignMod :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignMod orc maxCost (a1 `Ac` p1) (a2 `Ac` p2) =
  case testEquality a1 a2 of
    Just Refl -> Amod (Contract (a1, a2)) <$> local (liftPath (M:)) (alignO orc maxCost p1 p2)
    Nothing -> empty

-- alignAll :: (MonadOracle o m)
--            => o -> All Usingl p1 -> All Usingl p2
--            -> HistoryM m (Al TrivialA p1 p2)
-- alignAll orc x y = alignIns orc x y <|> alignDel orc x y <|> alignMod orc x y

-- Library stuff
newtype Contract (f :: k -> *) (x :: k) = Contract { unContract :: (f x , f x) }

type TrivialA = Contract Usingl
data TrivialP :: [U] -> [U] -> * where
 Pair :: All Usingl l -> All Usingl r -> TrivialP l r
  deriving (Generic)
--
-- mkEnv :: [Path] -> History
-- mkEnv p = History { path = p, deOpt = False }

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

unsafeCoerceEquality :: Usingl a -> Usingl b -> Maybe (a :~: b)
unsafeCoerceEquality a b = unsafeCoerce $ Just Refl
-- Show instances
deriving instance Show (Almu u v)
deriving instance Show (Al (At AlmuH) p1 p2)
deriving instance Show (At AlmuH u)
deriving instance Show (Spine (At AlmuH) (Al (At AlmuH)) u)
deriving instance Show (All Usingl l)
deriving instance Show (All (At AlmuH) l)
deriving instance Show (Ctx (AtmuPos u) p)
deriving instance Show (Ctx (AtmuNeg u) p)
deriving instance Show (f x) => Show (Contract f x)

-- Eq instances
instance Eq (Almu u v) where
  (Alspn s1) == (Alspn s2) = s1 == s2
  (Alins c1 ctx1) == (Alins c2 ctx2) = case testEquality c1 c2 of
    Nothing -> False
    Just Refl -> ctx1 == ctx2
  (Aldel c1 ctx1) == (Aldel c2 ctx2) = case testEquality c1 c2 of
    Nothing -> False
    Just Refl -> ctx1 == ctx2
  _ == _ = False

instance Eq (Spine (At AlmuH) (Al (At AlmuH)) u) where
  Scp == Scp = True
  (Scns c1 p1) == (Scns c2 p2) = case testEquality c1 c2 of
    Nothing -> False
    Just Refl -> p1 == p2
  (Schg i1 j1 p1) == (Schg i2 j2 p2) = case testEquality i1 i2 of
    Nothing -> False
    Just Refl -> case testEquality j1 j2 of
      Nothing -> False
      Just Refl -> p1 == p2
  _ == _ = False

deriving instance Eq (Ctx (AtmuPos u) p)
deriving instance Eq (Ctx (AtmuNeg u) p)
deriving instance Eq (All Usingl l)
deriving instance Eq (AtmuPos u v)
deriving instance Eq (AtmuNeg u v)
deriving instance Eq (AlmuH u)
deriving instance Eq (Al (At AlmuH) p1 p2)
deriving instance Eq (All (At AlmuH) l)
deriving instance Eq (At AlmuH u)
deriving instance Eq (TrivialA u)


showAll :: All (At AlmuH) l -> String
showAll An = ""
showAll (x `Ac` xs) = show x ++ show xs

showSpine :: Spine (At AlmuH) (Al (At AlmuH)) u -> String
showSpine (Scp) = "Scp. "
showSpine (Scns i p) = "Scns " ++ show i ++ " (" ++ show p ++ ") "
showSpine (Schg i j p) = "Schg From: " ++ show i ++ " to " ++ show j ++ showAl p

showAlmu :: Almu u v -> String
showAlmu (Alspn s) = "M-" ++ show s
showAlmu (Alins c d) = "I-{" ++ show c ++ "}" ++ show d
showAlmu (Aldel c d) = "D-{" ++ show c ++ "}" ++ show d

showCtxP :: Ctx (AtmuPos u) p -> String
showCtxP (Here r p) = show r
showCtxP (There u c) = show c

showCtxN :: Ctx (AtmuNeg u) p -> String
showCtxN (Here r p) = show r
showCtxN (There u c) = show c

showAt :: At AlmuH u -> String
showAt (Ai r) = show r
showAt (As t) = show t

showAl :: Al (At AlmuH) p1 p2 -> String
showAl A0 = ""
showAl (Ains i al) = "+{" ++ show i ++ "}" ++ showAl al
showAl (Adel d al) = "-{" ++ show d ++ "}" ++ showAl al
showAl (Amod m al) = "%{" ++ show m ++ "}" ++ showAl al

showNonRecAl :: Al TrivialA s d -> String
showNonRecAl A0 = ""
showNonRecAl (Ains i al) = "+{" ++ show i ++ "}" ++ showNonRecAl al
showNonRecAl (Adel d al) = "-{" ++ show d ++ "}" ++ showNonRecAl al
showNonRecAl (Amod m al) = "%{" ++ show m ++ "}" ++ showNonRecAl al

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
import Data.Constraint (Constraint, (:-), (\\), Dict(..))
import Data.Constraint.Forall (Forall, ForallF, ForallT, inst, instF, instT)

import Debug.Trace

import Language.Common
import Language.Clojure.Lang

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

data PartialAlmu :: U -> U -> * where
  TSpn :: Spine (At (AlmuP)) (Al (At (AlmuP))) u
        -> PartialAlmu u u
  TIns :: ConstrFor v s -> Ctx (PartialPos u) (TypeOf s)
        -> PartialAlmu u v
  TDel :: ConstrFor u s -> Ctx (PartialNeg v) (TypeOf s)
        -> PartialAlmu u v

data AlmuP :: U -> * where
  AlmuBase :: TrivialA u -> AlmuP u
  AlmuF :: f (PartialAlmu u u) -> AlmuP u

data AlmuH :: U -> * where
  AlmuH :: Almu u u -> AlmuH u
  deriving (Show, Generic)

unH :: AlmuH u -> Almu u u
unH (AlmuH u) = u

data PartialPos (v :: U) :: U -> * where
  TPos :: (Usingl v, Usingl u) -> PartialPos v u

data PartialNeg (v :: U) :: U -> * where
  TNeg :: (Usingl u, Usingl v) -> PartialNeg v u

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

-- useful wrappers for instances
data Predicate (a :: * -> Constraint) = Predicate
data Proxy (a :: *) = Proxy

proxy :: a -> Proxy a
proxy _ = Proxy

inst_ :: Predicate p -> Proxy a -> Forall p :- p a
inst_ _ _ = inst

instF_ :: Predicate p -> Proxy (u a) -> ForallF p u :- p (u a)
instF_ _ _ = instF
-- Show instances
deriving instance Show (Almu u v)
deriving instance Show (Al (At AlmuH) p1 p2)
deriving instance Show (At AlmuH u)
deriving instance Show (Spine (At AlmuH) (Al (At AlmuH)) u)
deriving instance Show (All Usingl l)
deriving instance Show (All (At AlmuH) l)
deriving instance Show (Ctx (AtmuPos u) p)
deriving instance Show (Ctx (Almu u) p)
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

instance (ForallF Eq p) => Eq (Spine p (Al p) u) where
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

equality :: Predicate Eq
equality = Predicate

-- withEqualityOf :: (Eq (u a) => c) -> (u a) -> c
withEqualityOf e p = e \\ instF_ equality (proxy p)

-- deriving instance Eq (Ctx (AtmuPos u) p)
instance (ForallF Eq almu) => Eq (Ctx almu p) where
  (Here al1 rest1) == (Here al2 rest2)
    = (al1 == al1 \\ instF_ equality (proxy al1)) && rest1 == rest2
  (There u1 ctx1)  == (There u2 ctx2)
    = (u1 == u2 && ctx1 == ctx2)
-- deriving instance Eq (Ctx (AtmuNeg u) p)
instance (ForallF Eq u) => Eq (All u l) where
  (u1 `Ac` us1) == (u2 `Ac` us2)
    = (u1 == u2 \\ instF_ equality (proxy u1)) && us1 == us2
  An            == An            = True
deriving instance Eq (AtmuPos u v)
deriving instance Eq (AtmuNeg u v)
deriving instance Eq (AlmuH u)
instance (ForallF Eq p) => Eq (Al p p1 p2) where
  A0 == A0 = True
  (Ains p1 rest1) == (Ains p2 rest2) = p1 == p2 && rest1 == rest2
  (Amod m1 rest1) == (Amod m2 rest2)
    = (m1 == m2 \\ instF_ equality (proxy m1)) && rest1 == rest2

-- deriving instance Eq (All (At AlmuH) l)
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

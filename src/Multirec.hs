{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module MultiRec where

import Data.Kind
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Data.Type.Equality hiding (apply)
import Control.Applicative




-- Our Sexprs
data SExpr = List SExprList | Operation SExpr SExpr | Value Int
  deriving (Eq, Show)

data SExprList = SNil | SCons SExpr SExprList
  deriving (Eq, Show)

data U = KInt | KSExpr | KSExprList
  deriving (Eq , Show)

data Usingl :: U -> * where
  Uint    :: Int       -> Usingl KInt
  Usexpr  :: SExpr     -> Usingl KSExpr
  UsexprL :: SExprList -> Usingl KSExprList

-- All constructors of the mutually recursive family
data Constr :: * where
  C1List :: Constr
  C1Operation :: Constr
  C1Value :: Constr
  C2SNil :: Constr
  C2SCons :: Constr

-- Which type of the family is constructed by which constructor
data ConstrFor :: U -> Constr -> * where
  C1ListProof      :: ConstrFor KSExpr C1List
  C1OperationProof :: ConstrFor KSExpr C1Operation
  C1ValueProof     :: ConstrFor KSExpr C1Value
  C2SNilProof      :: ConstrFor KSExprList C2SNil
  C2SConsProof     :: ConstrFor KSExprList C2SCons

-- Library stuff
newtype Contract (f :: k -> *) (x :: k) = Contract { unContract :: (f x , f x) }

type family TypeOf (c :: Constr) :: [U] where
  TypeOf C1List      = '[KSExprList]
  TypeOf C1Operation = '[KSExpr, KSExpr]
  TypeOf C1Value     = '[KInt]
  TypeOf C2SNil      = '[]
  TypeOf C2SCons     = '[KSExpr, KSExprList]

type Trivial = Contract
type TrivialA = Contract Usingl
data TrivialP :: [U] -> [U] -> * where
 Pair :: All Usingl l -> All Usingl r -> TrivialP l r
-- Thr actual puzzle

data All (p :: k -> *) :: [k] -> * where
  An :: All p '[]
  Ac :: p x -> All p xs -> All p (x ': xs)

(.@.) :: p x -> All p xs -> All p (x ': xs)
(.@.) = Ac
infixr 2 .@.
infixr 2 `Ac`

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

class IsRecEl (u :: U) where
instance IsRecEl KSExpr where
instance IsRecEl KSExprList where

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

align :: All Usingl p1 -> All Usingl p2 -> [Al TrivialA p1 p2]
align An           An           = return A0
align An           (a `Ac` p)   = Ains a <$> align An p
align (a `Ac` p)   An           = Adel a <$> align p An
align (a1 `Ac` p1) (a2 `Ac` p2) = case testEquality a1 a2 of
  Just Refl -> Amod (Contract (a1, a2)) <$> align p1 p2
           <|> Adel a1 <$> align p1 (a2 `Ac` p2)
           <|> Ains a2 <$> align (a1 `Ac` p1) p2

  Nothing   -> Adel a1 <$> align p1 (a2 `Ac` p2)
           <|> Ains a2 <$> align (a1 `Ac` p1) p2

-- Library stuff

inj :: ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj C1ListProof      (l `Ac` An)         = Usexpr (List (eval l))
inj C1OperationProof (a `Ac` b `Ac` An)        = Usexpr (Operation (eval a) (eval b))
inj C1ValueProof     (v `Ac` An)         = Usexpr (Value (eval v))
inj C2SNilProof      An                  = UsexprL SNil
inj C2SConsProof     (s `Ac` sl `Ac` An) = UsexprL (SCons (eval s) (eval sl))

type family El (u :: U)  where
 El KSExpr = SExpr
 El KInt = Int
 El KSExprList = SExprList

eval :: Usingl u -> El u
eval (Usexpr s)   = s
eval (Uint k)     = k
eval (UsexprL sl) = sl

instance TestEquality (ConstrFor u) where
  testEquality C1ListProof C1ListProof           = Just Refl
  testEquality C1OperationProof C1OperationProof = Just Refl
  testEquality C1ValueProof C1ValueProof = Just Refl
  testEquality C2SNilProof C2SNilProof  = Just Refl
  testEquality C2SConsProof C2SConsProof = Just Refl
  testEquality _ _                       = Nothing

instance TestEquality Usingl where
  testEquality (Uint i) (Uint j)       = Just Refl
  testEquality (Usexpr _) (Usexpr _)   = Just Refl
  testEquality (UsexprL _) (UsexprL _) = Just Refl
  testEquality _ _ = Nothing

instance Eq (Usingl a) where
 (Uint i)    == (Uint j)        = i   == j
 (Usexpr s1) == (Usexpr s2)     = s1  == s2
 (UsexprL sl1) == (UsexprL sl2) = sl1 == sl2


data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (Usexpr s) = viewS s
view (UsexprL sl) = viewSL sl

viewS :: SExpr -> View KSExpr
viewS (List l)       = Tag C1ListProof (UsexprL l .@. An)
viewS (Operation a b) = Tag C1OperationProof (Usexpr a .@. Usexpr b .@. An)
viewS (Value v)      = Tag C1ValueProof (Uint v .@. An)

viewSL :: SExprList -> View KSExprList
viewSL SNil         = Tag C2SNilProof An
viewSL (SCons s sl) = Tag C2SConsProof (Usexpr s .@. UsexprL sl .@. An)
-- Utility

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


-- Show instanes


instance Show (Usingl u) where
 show (Uint i)  = show i
 show (Usexpr s) = show s
 show (UsexprL sl)    = show sl

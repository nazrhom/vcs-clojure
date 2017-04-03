{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module RegularTypes where

import Lang
import Data.Type.Equality hiding (apply)
import Control.Applicative

-- Library stuff

type family TypeOf (c :: Constr) :: [U] where
  TypeOf CAdd = '[KI, KI]
  TypeOf CSquare = '[KI]
  TypeOf CValue = '[KInt]

data View where
  Tag :: SExprConstr c -> All Usingl (TypeOf c) -> View

type family El (u :: U) = r | r -> u where
  El KI = SExpr
  El KInt = Int
  El KChar = Char

eval :: Usingl u -> El u
eval (Ui i) = i
eval (Uint k) = k
eval (Uchar c) = c

view :: Usingl KI -> View
view (Ui s) = viewS s

viewS :: SExpr -> View
viewS (Add a b) = Tag S_Add (Ui a .@. Ui b .@. An)
viewS (Square a) = Tag S_Square (Ui a .@. An)
viewS (Value v) = Tag S_Value (Uint v .@. An)

type Trivial = Contract

type TrivialA = Usingl
data TrivialP :: [U] -> [U] -> * where
  Pair :: All Usingl l -> All Usingl r -> TrivialP l r

newtype Contract (f :: k -> *) (x :: k) = Contract { unContract :: (f x , f x) }

(.@.) :: p x -> All p xs -> All p (x ': xs)
(.@.) = Ac
infixr 2 .@.
infixr 2 `Ac`

mapSpineM :: Monad m => (forall a . Contract at1 a -> m (Contract at2 a))
      -> (forall s d . al1 s d -> m (al2 s d))
      -> Spine at1 al1 -> m (Spine at2 al2)
mapSpineM f g Scp             = return Scp
mapSpineM f g (Scns c ps)     = Scns c <$> mapAllM f ps
mapSpineM f g (Schg c1 c2 al) = Schg c1 c2 <$> g al

mapAlM :: Monad m => forall (at1 :: U -> *) (at2 :: U -> *) .
      (forall a . at1 a -> m (at2 a))
      -> Al at1 s d -> m (Al at2 s d)
mapAlM f A0 = return A0
mapAlM f (Adel at al) = Adel at <$> mapAlM f al
mapAlM f (Ains at al) = Ains at <$> mapAlM f al
mapAlM f (Amod at al) = Amod <$> f at <*> mapAlM f al

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

-- Instances
instance TestEquality Usingl where
  testEquality (Uint _) (Uint _) = Just Refl
  testEquality (Uchar _) (Uchar _) = Just Refl
  testEquality (Ui _) (Ui _) = Just Refl
  testEquality _ _ = Nothing

instance Eq (Usingl a) where
  (Uint i) == (Uint j) = i == j
  _ == _               = False

instance Show (Spine k p) where
  show Scp = "Copy"
  show (Scns i p) = "Cns " ++ show i
  show (Schg i j p) = "Chg " ++ show i ++ " " ++ show j

instance Show (Usingl u) where
  show (Uint i)  = show i
  show (Uchar c) = show c
  show (Ui s)    = show s

-- The actual puzzle

data U = KInt | KChar | KI
  deriving (Eq , Show)

data Usingl :: U -> * where
  Uint  :: Int -> Usingl KInt
  Uchar :: Char -> Usingl KChar
  Ui    :: SExpr -> Usingl KI

data All (p :: k -> *) :: [k] -> * where
  An :: All p '[]
  Ac :: p x -> All p xs -> All p (x ': xs)


data Spine (at :: U -> *)(al :: [U] -> [U] -> *) :: * where
  Scp  :: Spine at al
  Scns :: SExprConstr s -> All (Contract at) (TypeOf s) -> Spine at al
  Schg :: SExprConstr s -> SExprConstr r
       -> al (TypeOf s) (TypeOf r)
       -> Spine at al

data Al (at :: U -> *) :: [U] -> [U] -> * where
  A0   :: Al at '[] '[]
  Ains :: Usingl u -> Al at xs ys -> Al at xs (u ': ys)
  Adel :: Usingl u -> Al at xs ys -> Al at (u ': xs) ys
  Amod :: at u -> Al at xs ys -> Al at (u ': xs) (u ': ys)

data At (recP :: *) :: U -> * where
  Ai :: recP -> At recP KI
  As :: Trivial Usingl u -> At recP u

data Almu :: * where
  Alspn :: Spine (At Almu) (Al (At Almu)) -> Almu
  Alins :: SExprConstr s -> Ctx (TypeOf s) -> Almu
  Aldel :: SExprConstr s -> Ctx (TypeOf s) -> Almu

data Ctx :: [U] -> * where
  Here :: Almu -> All Usingl l -> Ctx (KI ': l)
  There :: Usingl u -> Ctx l -> Ctx (u ': l)

spine :: Usingl KI -> Usingl KI -> Spine TrivialA TrivialP
spine x y | x == y = Scp
spine x y | otherwise = case (view x, view y) of
  ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
    Just Refl -> Scns c1 (zipP l1 l2)
    Nothing -> Schg c1 c2 (Pair l1 l2)

align :: All Usingl p1 -> All Usingl p2 -> [Al (Contract Usingl) p1 p2]
align An           An           = return A0
align An           (a `Ac` p)   = Ains a <$> align An p
align (a `Ac` p)   An           = Adel a <$> align p An
align (a1 `Ac` p1) (a2 `Ac` p2) = case testEquality a1 a2 of
  Just Refl -> Amod (Contract (a1, a2)) <$> align p1 p2
           <|> Adel a1 <$> align p1 (a2 `Ac` p2)
           <|> Ains a2 <$> align (a1 `Ac` p1) p2

  Nothing   -> Adel a1 <$> align p1 (a2 `Ac` p2)
           <|> Ains a2 <$> align (a1 `Ac` p1) p2

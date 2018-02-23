{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Language.Presentation.Lang where

import Language.Common
import Language.Presentation.AST

import Data.Type.Equality hiding (apply)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Data.Proxy


data U = KT | KA | KInt -- Universe

data Constr = CSum | CAtom | CParens | CVal -- Sum
  deriving (Show)

data ConstrFor :: U -> Constr -> * where
  SumProof          :: ConstrFor KT CSum
  AtomProof         :: ConstrFor KT CAtom

  ParensProof       :: ConstrFor KA CParens
  ValProof          :: ConstrFor KA CVal
deriving instance Show (ConstrFor u c)

type family TypeOf (c :: Constr) :: [U] where
  TypeOf CSum = '[KT, KT]
  TypeOf CAtom = '[KA]
  TypeOf CParens = '[KT]
  TypeOf CVal = '[KInt]

data Usingl :: U -> * where
  UInt :: Int -> Usingl KInt
  UT   :: T   -> Usingl KT
  UA   :: A   -> Usingl KA
deriving instance Show (Usingl u)

class IsRecEl (u :: U) where
instance IsRecEl KT where
instance IsRecEl KA where
instance {-# OVERLAPPABLE #-} (TypeError (Text "Not a recursive guy: " :<>: ShowType s)) => IsRecEl s

onRecursiveGuy :: ((IsRecEl v) => Usingl v -> a) -> (Usingl v -> a) -> Usingl v -> a
onRecursiveGuy rec nonrec at@(UInt _) = nonrec at
onRecursiveGuy rec nonrec at@(UT _) = rec at
onRecursiveGuy rec nonrec at@(UA _) = rec at

inj :: IsRecEl r => ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj SumProof (t1 `Ac` t2 `Ac` An) = UT (Sum (fromSing t1) (fromSing t2))
inj AtomProof (i `Ac` An) = UT (Atom (fromSing i))
inj ParensProof (i `Ac` An) = UA (Parens (fromSing i))
inj ValProof (i `Ac` An) = UA (Val (fromSing i))

data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (UT t) = viewT t
  where
    viewT (Sum t1 t2) = Tag SumProof (UT t1 .@. UT t2 .@. An)
    viewT (Atom i)     = Tag AtomProof (UA i .@. An)
view (UA a) = viewA a
  where
    viewA (Parens t) = Tag ParensProof (UT t .@. An)
    viewA (Val i)    = Tag ValProof (UInt i .@. An)

type family ToSing (k :: *) :: U where
  ToSing T = KT
  ToSing A = KA
  ToSing Int = KInt

type family FromSing (u :: U) where
  FromSing KT = T
  FromSing KA = A
  FromSing KInt = Int

class Sing a where
  toSing :: a -> Usingl (ToSing a)
  fromSing :: Usingl (ToSing a) -> a

instance Sing T where
  toSing a = UT a
  fromSing (UT t) = t

instance Sing A where
  toSing a = UA a
  fromSing (UA t) = t

instance Sing Int where
  toSing i = UInt i
  fromSing (UInt i) = i

instance TestEquality (ConstrFor u) where
  testEquality a b = case testEquality' a b of
    Just (Refl, Refl) -> Just Refl
    Nothing -> Nothing

testEquality' :: ConstrFor a b -> ConstrFor c d -> Maybe ((a :~: c), (b :~: d))
testEquality' SumProof SumProof = Just (Refl, Refl)
testEquality' AtomProof AtomProof = Just (Refl, Refl)
testEquality' ParensProof ParensProof = Just (Refl, Refl)
testEquality' ValProof ValProof = Just (Refl, Refl)
testEquality' _         _         = Nothing

instance TestEquality Usingl where
  testEquality (UInt _) (UInt _) = Just Refl
  testEquality (UT _) (UT _) = Just Refl
  testEquality (UA _) (UA _) = Just Refl
  testEquality _ _ = Nothing

instance Eq (Usingl a) where
  (UInt a) == (UInt b) = a == b
  (UT a) == (UT b) = a == b
  (UA a) == (UA b) = a == b
  _ == _ = True

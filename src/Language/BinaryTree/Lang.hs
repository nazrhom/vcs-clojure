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
module Language.BinaryTree.Lang where

import Language.Common
import Language.BinaryTree.AST

import Data.Type.Equality hiding (apply)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Data.Proxy


data U =
    KInt
  | KIntTree

data Constr :: * where
  CNode :: Constr
  CLeaf :: Constr
  deriving (Show)

data ConstrFor :: U -> Constr -> * where
  NodeProof :: ConstrFor KIntTree CNode
  LeafProof :: ConstrFor KIntTree CLeaf
deriving instance Show (ConstrFor u c)

type family TypeOf (c :: Constr) :: [U] where
  TypeOf CNode = '[KIntTree, KIntTree]
  TypeOf CLeaf = '[KInt]

data Usingl :: U -> * where
  UInt :: Int -> Usingl KInt
  UIntTree :: IntTree -> Usingl KIntTree
deriving instance Show (Usingl u)

class IsRecEl (u :: U) where
instance IsRecEl KIntTree where
instance {-# OVERLAPPABLE #-} (TypeError (Text "Not a recursive guy: " :<>: ShowType s))
         => IsRecEl s

onRecursiveGuy :: ((IsRecEl v) => Usingl v -> a) -> (Usingl v -> a) -> Usingl v -> a
onRecursiveGuy rec nonrec at@(UInt _) = nonrec at
onRecursiveGuy rec nonrec at@(UIntTree _) = rec at

inj :: ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj NodeProof (t1 `Ac` t2 `Ac` An) = UIntTree (Node (fromSing t1) (fromSing t2))
inj LeafProof (i `Ac` An) = UIntTree (Leaf (fromSing i))

data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (UIntTree t) = viewTree t
  where
    viewTree (Node t1 t2) = Tag NodeProof (UIntTree t1 .@. UIntTree t2 .@. An)
    viewTree (Leaf i)     = Tag LeafProof (UInt i .@. An)

type family ToSing (k :: *) :: U where
  ToSing IntTree = KIntTree
  ToSing Int     = KInt

type family FromSing (u :: U) where
  FromSing KIntTree = IntTree
  FromSing KInt     = Int

class Sing a where
  toSing :: a -> Usingl (ToSing a)
  fromSing :: Usingl (ToSing a) -> a

instance Sing IntTree where
  toSing a = UIntTree a
  fromSing (UIntTree t) = t

instance Sing Int where
  toSing i = UInt i
  fromSing (UInt i) = i

instance TestEquality (ConstrFor u) where
  testEquality a b = case testEquality' a b of
    Just (Refl, Refl) -> Just Refl
    Nothing -> Nothing

testEquality' :: ConstrFor a b -> ConstrFor c d -> Maybe ((a :~: c), (b :~: d))
testEquality' NodeProof NodeProof = Just (Refl, Refl)
testEquality' LeafProof LeafProof = Just (Refl, Refl)
testEquality' _         _         = Nothing

instance TestEquality Usingl where
  testEquality (UInt _) (UInt _) = Just Refl
  testEquality (UIntTree _) (UIntTree _) = Just Refl
  testEquality _ _ = Nothing

instance Eq (Usingl a) where
  (UInt a) == (UInt b) = a == b
  (UIntTree a) == (UIntTree b) = a == b
  _ == _ = True



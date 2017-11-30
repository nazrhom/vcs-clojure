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
import Data.Type.Equality hiding (apply)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Data.Proxy

data IntTree = Node IntTree IntTree | Leaf Int

data U =
    KInt
  | KIntTree

data Constr :: * where
  CNode :: Constr
  CLeaf :: Constr

data ConstrFor :: U -> Constr -> * where
  NodeProof :: ConstrFor KIntTree CNode
  LeafProof :: ConstrFor KIntTree CLeaf

type family TypeOf (c :: Constr) :: [U] where
  TypeOf CNode = '[KIntTree, KIntTree]
  TypeOf CLeaf = '[KInt]

data Usingl :: U -> * where
  UInt :: Int -> Usingl KInt
  UIntTree :: IntTree -> Usingl KIntTree

class IsRecEl (u :: U) where
instance IsRecEl KIntTree where
instance {-# OVERLAPPABLE #-} (TypeError (Text "Not a recursive guy: " :<>: ShowType s))
         => IsRecEl s

inj :: ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj NodeProof (t1 `Ac` t2 `Ac` An) = UIntTree (Node (fromSing t1) (fromSing t2))
inj LeafProof (i `Ac` An) = UIntTree (Leaf (fromSing i))

data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (UIntTree t) = viewTree t
  where
    viewTree (Node t1 t2) = Tag NodeProof (UIntTree t1 .@. UIntTree t2 .@. An)

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

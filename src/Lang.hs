{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Lang where

import Data.Type.Equality hiding (apply)
import GHC.TypeLits (ErrorMessage(..), TypeError)
-- Lang stuff

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

type family TypeOf (c :: Constr) :: [U] where
  TypeOf C1List      = '[KSExprList]
  TypeOf C1Operation = '[KSExpr, KSExpr]
  TypeOf C1Value     = '[KInt]
  TypeOf C2SNil      = '[]
  TypeOf C2SCons     = '[KSExpr, KSExprList]

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

class IsRecEl (u :: U) where
instance IsRecEl KSExpr where
instance IsRecEl KSExprList where
data All (p :: k -> *) :: [k] -> * where
  An :: All p '[]
  Ac :: p x -> All p xs -> All p (x ': xs)

(.@.) :: p x -> All p xs -> All p (x ': xs)
(.@.) = Ac
infixr 2 .@.
infixr 2 `Ac`

-- Show instances

instance Show (Usingl u) where
 show (Uint i)  = show i
 show (Usexpr s) = show s
 show (UsexprL sl)    = show sl

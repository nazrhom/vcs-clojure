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
module Clojure.Lang where

import Data.Type.Equality hiding (apply)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Clojure.AST
import Clojure.PrettyPrint


-- UNIVERSE
data All (p :: k -> *) :: [k] -> * where
  An :: All p '[]
  Ac :: p x -> All p xs -> All p (x ': xs)

(.@.) :: p x -> All p xs -> All p (x ': xs)
(.@.) = Ac
infixr 2 .@.
infixr 2 `Ac`

data U =
    KString
  | KSepExprList
  | KExpr
  | KTerm

data Usingl :: U -> * where
  UString :: String -> Usingl KString
  USepExprList :: SepExprList -> Usingl KSepExprList
  UExpr :: Expr -> Usingl KExpr
  UTerm :: Term -> Usingl KTerm

data Constr :: * where
  C1Nil  :: Constr
  C1Cons :: Constr

  C3Special  :: Constr
  C3Dispatch  :: Constr
  C3Collection  :: Constr
  C3Term  :: Constr
  C3Comment :: Constr
  C3Seq      :: Constr
  C3Empty    :: Constr

  C6TaggedString :: Constr
  deriving Show

data ConstrFor :: U -> Constr -> * where
  C1NilProof :: ConstrFor KSepExprList C1Nil
  C1ConsProof :: ConstrFor KSepExprList C1Cons

  C3SpecialProof :: ConstrFor KExpr C3Special
  C3DispatchProof :: ConstrFor KExpr C3Dispatch
  C3CollectionProof :: ConstrFor KExpr C3Collection
  C3TermProof :: ConstrFor KExpr C3Term
  C3CommentProof :: ConstrFor KExpr C3Comment
  C3SeqProof :: ConstrFor KExpr C3Seq
  C3EmptyProof :: ConstrFor KExpr C3Empty

  C6TaggedStringProof :: ConstrFor KTerm C6TaggedString

deriving instance Show (ConstrFor u c)

showConstr :: ConstrFor u c -> String
showConstr C1NilProof = "Nil"
showConstr C1ConsProof = "Cons"
showConstr C3SpecialProof = "Special"
showConstr C3DispatchProof = "Dispatch"
showConstr C3CollectionProof = "Collection"
showConstr C3TermProof = "Term"
showConstr C3CommentProof = "Comment"
showConstr C3SeqProof = "Seq"
showConstr C3EmptyProof = "Empty"
showConstr C6TaggedStringProof = "TaggedString"

type family TypeOf (c :: Constr) :: [U] where
  TypeOf C1Nil = '[]
  TypeOf C1Cons = '[KExpr, KString, KSepExprList]

  TypeOf C3Special = '[KString, KExpr]
  TypeOf C3Dispatch = '[KExpr]
  TypeOf C3Collection = '[KString, KSepExprList]
  TypeOf C3Term = '[KTerm]
  TypeOf C3Comment = '[KString]
  TypeOf C3Seq = '[KExpr, KExpr]
  TypeOf C3Empty = '[]

  TypeOf C6TaggedString = '[KString, KString]


class IsRecEl (u :: U) where
instance IsRecEl KExpr where
instance IsRecEl KSepExprList where
instance IsRecEl KTerm where
instance {-# OVERLAPPABLE #-} (TypeError (Text "Not a recursive guy: " :<>: ShowType s))
         => IsRecEl s

-- Library stuff
inj :: ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj C1NilProof An = USepExprList (Nil emptyRange)
inj C1ConsProof (e `Ac` sep `Ac` sl `Ac` An) = USepExprList (Cons (eval e) (eval sep) (eval sl) emptyRange)

inj C3SpecialProof (fty `Ac` e `Ac` An) = UExpr (Special (eval fty) (eval e) emptyRange)
inj C3DispatchProof (e `Ac` An) = UExpr (Dispatch (eval e) emptyRange)
inj C3CollectionProof (ct `Ac` sl `Ac` An) = UExpr (Collection (eval ct) (eval sl) emptyRange)
inj C3TermProof (t `Ac` An) = UExpr (Term (eval t) emptyRange)
inj C3CommentProof (c `Ac` An) = UExpr (Comment (eval c) emptyRange)
inj C3SeqProof (p `Ac` q `Ac` An) = UExpr (Seq (eval p) (eval q) emptyRange)
inj C3EmptyProof An = UExpr (Empty emptyRange)

inj C6TaggedStringProof (t `Ac` s `Ac` An) = UTerm (TaggedString (eval t) (eval s) emptyRange)

extractRange :: Usingl r -> Maybe LineRange
extractRange (UExpr e) = Just (extractRangeExpr e)
extractRange (USepExprList sl) = Just (extractRangeSepExprList sl)
extractRange (UTerm t) = Just (extractRangeTerm t)
extractRange (UString s) = Nothing

type family El (u :: U) where
  El KString = String
  El KSepExprList = SepExprList
  El KExpr = Expr
  El KTerm = Term

eval :: Usingl u -> El u
eval (UString u) = u
eval (USepExprList u) = u
eval (UExpr u) = u
eval (UTerm u) = u

instance TestEquality (ConstrFor u) where
  testEquality a b = case testEquality' a b of
    Just (Refl, Refl) -> Just Refl
    Nothing -> Nothing

testEquality' :: ConstrFor a b -> ConstrFor c d -> Maybe ((a :~: c), (b :~: d))
testEquality' C1NilProof C1NilProof = Just (Refl, Refl)
testEquality' C1ConsProof C1ConsProof = Just (Refl, Refl)

testEquality' C3SpecialProof C3SpecialProof = Just (Refl, Refl)
testEquality' C3DispatchProof C3DispatchProof = Just (Refl, Refl)
testEquality' C3CollectionProof C3CollectionProof = Just (Refl, Refl)
testEquality' C3TermProof C3TermProof = Just (Refl, Refl)
testEquality' C3CommentProof C3CommentProof = Just (Refl, Refl)
testEquality' C3SeqProof C3SeqProof = Just (Refl, Refl)
testEquality' C3EmptyProof C3EmptyProof = Just (Refl, Refl)

testEquality' C6TaggedStringProof C6TaggedStringProof = Just (Refl, Refl)

testEquality' _ _ = Nothing



instance TestEquality Usingl where
  testEquality (UString _) (UString _) = Just Refl
  testEquality (USepExprList _) (USepExprList _) = Just Refl
  testEquality (UExpr _) (UExpr _) = Just Refl
  testEquality (UTerm _) (UTerm _) = Just Refl
  testEquality _ _ = Nothing

instance Eq (Usingl a) where
  (UString a) == (UString b) = a == b
  (USepExprList a) == (USepExprList b) = a == b
  (UExpr a) == (UExpr b) = a == b
  (UTerm a) == (UTerm b) = a == b
  _ == _ = True

data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (UExpr expr) = viewExpr expr
view (USepExprList sepexprlist) = viewSepExprList sepexprlist
view (UTerm term) = viewTerm term

viewSepExprList :: SepExprList -> View KSepExprList
viewSepExprList (Nil _) = Tag C1NilProof An
viewSepExprList (Cons e s sl _) = Tag C1ConsProof (UExpr e .@. UString s .@. USepExprList sl .@. An)

viewExpr :: Expr -> View KExpr
viewExpr (Special fty e _) = Tag C3SpecialProof (UString fty .@. UExpr e .@. An)
viewExpr (Dispatch e _) = Tag C3DispatchProof (UExpr e .@. An)
viewExpr (Collection ct sl _) = Tag C3CollectionProof (UString ct .@. USepExprList sl .@. An)
viewExpr (Term t _) = Tag C3TermProof (UTerm t .@. An)
viewExpr (Comment c _) = Tag C3CommentProof (UString c .@. An)
viewExpr (Seq p q _)  = Tag C3SeqProof (UExpr p .@. UExpr q .@. An)
viewExpr (Empty _)  = Tag C3EmptyProof An


viewTerm :: Term -> View KTerm
viewTerm (TaggedString t s _) = Tag C6TaggedStringProof (UString t .@. UString s .@. An)


onRecursiveGuy :: ((IsRecEl v) => Usingl v -> a) -> (Usingl v -> a) -> Usingl v -> a
onRecursiveGuy rec nonrec at@(UString _) = nonrec at
onRecursiveGuy rec nonrec at@(USepExprList _) = rec at
onRecursiveGuy rec nonrec at@(UExpr _) = rec at
onRecursiveGuy rec nonrec at@(UTerm _) = rec at

-- Utility
type family Le (k :: *) :: U where
  Le SepExprList = KSepExprList
  Le Expr = KExpr
  Le Term = KTerm
  Le String = KString

class Sing a where
  toSing :: a -> Usingl (Le a)

instance Sing SepExprList where
  toSing (Nil r) = USepExprList (Nil r)
  toSing (Cons e s sl r) = USepExprList (Cons e s sl r)

instance Sing Expr where
  toSing (Special fty e r) = UExpr (Special fty e r)
  toSing (Dispatch e r) = UExpr (Dispatch e r)
  toSing (Collection ct sl r) = UExpr (Collection ct sl r)
  toSing (Term t r) = UExpr (Term t r)
  toSing (Comment c r) = UExpr (Comment c r)
  toSing (Seq p q r) = UExpr (Seq p q r)
  toSing (Empty r) = UExpr (Empty r)

instance Sing Term where
  toSing (TaggedString t s r) = UTerm (TaggedString t s r)

instance Sing String where
  toSing s = UString s


-- instance Show (Usingl u) where
--   show (UString u) = show u
--   show (USepExprList u) = show $ ppSepExprList u
--   show (UExpr u) = show $ ppExpr u
--   show (UTerm u) = show $ ppTerm u
deriving instance Show (Usingl u)

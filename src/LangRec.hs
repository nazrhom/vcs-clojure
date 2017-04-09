{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module LangRec where

import Data.Type.Equality hiding (apply)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Parser
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
  C1Singleton  :: Constr
  C1Cons :: Constr

  C3Special  :: Constr
  C3Dispatch  :: Constr
  C3Collection  :: Constr
  C3Term  :: Constr
  C3Comment :: Constr

  C6TaggedString :: Constr


data ConstrFor :: U -> Constr -> * where
  C1NilProof :: ConstrFor  KSepExprList C1Nil
  C1SingletonProof :: ConstrFor  KSepExprList C1Singleton
  C1ConsProof :: ConstrFor KSepExprList C1Cons

  C3SpecialProof :: ConstrFor  KExpr C3Special
  C3DispatchProof :: ConstrFor  KExpr C3Dispatch
  C3CollectionProof :: ConstrFor  KExpr C3Collection
  C3TermProof :: ConstrFor  KExpr C3Term
  C3CommentProof :: ConstrFor KExpr C3Comment

  C6TaggedStringProof :: ConstrFor KTerm C6TaggedString



type family TypeOf (c :: Constr) :: [U] where
  TypeOf C1Nil = '[]
  TypeOf C1Singleton = '[KExpr]
  TypeOf C1Cons = '[KExpr, KString, KSepExprList]

  TypeOf C3Special = '[KString, KExpr]
  TypeOf C3Dispatch = '[KExpr]
  TypeOf C3Collection = '[KString, KSepExprList]
  TypeOf C3Term = '[KTerm]
  TypeOf C3Comment = '[KString]

  TypeOf C6TaggedString = '[KString, KString]


class IsRecEl (u :: U) where
instance IsRecEl KExpr where
instance IsRecEl KSepExprList where
instance IsRecEl KTerm where
instance {-# OVERLAPPABLE #-} (TypeError (Text "Not a recursive guy: " :<>: ShowType s))
         => IsRecEl s

-- Library stuff
inj :: ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj C1NilProof An = USepExprList Nil
inj C1SingletonProof (e `Ac` An) = USepExprList (Singleton (eval e))
inj C1ConsProof (e `Ac` sep `Ac` sl `Ac` An) = USepExprList (Cons (eval e) (eval sep) (eval sl))
inj C3SpecialProof (fty `Ac` e `Ac` An) = UExpr (Special (eval fty) (eval e))
inj C3DispatchProof (e `Ac` An) = UExpr (Dispatch (eval e))
inj C3CollectionProof (ct `Ac` sl `Ac` An) = UExpr (Collection (eval ct) (eval sl))
inj C3TermProof (t `Ac` An) = UExpr (Term (eval t))
inj C3CommentProof (c `Ac` An) = UExpr (Comment (eval c))
inj C6TaggedStringProof (t `Ac` s `Ac` An) = UTerm (TaggedString (eval t) (eval s))


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
  testEquality C1NilProof C1NilProof = Just Refl
  testEquality C1SingletonProof C1SingletonProof = Just Refl
  testEquality C1ConsProof C1ConsProof = Just Refl
  testEquality C3SpecialProof C3SpecialProof = Just Refl
  testEquality C3DispatchProof C3DispatchProof = Just Refl
  testEquality C3CollectionProof C3CollectionProof = Just Refl
  testEquality C3TermProof C3TermProof = Just Refl
  testEquality C3CommentProof C3CommentProof = Just Refl
  testEquality C6TaggedStringProof C6TaggedStringProof = Just Refl
  testEquality _ _ = Nothing

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

data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (UExpr expr) = viewExpr expr
view (USepExprList sepexprlist) = viewSepExprList sepexprlist
view (UTerm term) = viewTerm term

viewSepExprList :: SepExprList -> View KSepExprList
viewSepExprList Nil = Tag C1NilProof An
viewSepExprList (Singleton e) = Tag C1SingletonProof (UExpr e .@. An)
viewSepExprList (Cons e s sl) = Tag C1ConsProof (UExpr e .@. UString s .@. USepExprList sl .@. An)


viewExpr :: Expr -> View KExpr
viewExpr (Special fty e) = Tag C3SpecialProof (UString fty .@. UExpr e .@. An)
viewExpr (Dispatch e) = Tag C3DispatchProof  (UExpr e .@. An)
viewExpr (Collection ct sl) = Tag C3CollectionProof (UString ct .@. USepExprList sl .@. An)
viewExpr (Term t) = Tag C3TermProof (UTerm t .@. An)
viewExpr (Comment c) = Tag C3CommentProof (UString c .@. An)


viewTerm :: Term -> View KTerm
viewTerm (TaggedString t s) = Tag C6TaggedStringProof (UString t .@. UString s .@. An)



-- Utility
type family Le (k :: *) :: U where
  Le SepExprList = KSepExprList
  Le Expr = KExpr
  Le Term = KTerm

class Sing a where
  toSing :: a -> Usingl (Le a)

instance Sing SepExprList where
  toSing Nil = USepExprList Nil
  toSing (Singleton e) = USepExprList (Singleton e)
  toSing (Cons e s sl) = USepExprList (Cons e s sl)
instance Sing Expr where
  toSing (Special fty e) = UExpr (Special fty e)
  toSing (Dispatch e) = UExpr (Dispatch e)
  toSing (Collection ct sl) = UExpr (Collection ct sl)
  toSing (Term t) = UExpr (Term t)
  toSing (Comment c) = UExpr (Comment c)
instance Sing Term where
  toSing (TaggedString t s) = UTerm (TaggedString t s)


instance Show (Usingl u) where
  show (UString u) = show u
  show (USepExprList u) = show u
  show (UExpr u) = show u
  show (UTerm u) = show u

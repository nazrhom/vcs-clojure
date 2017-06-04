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
module Lang where

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
  | KSep
  | KSepExprList
  | KExpr
  | KFormTy
  | KCollType
  | KTerm
  | KTag

data Usingl :: U -> * where
  UString :: String -> Usingl KString
  USep :: Sep -> Usingl KSep
  USepExprList :: SepExprList -> Usingl KSepExprList
  UExpr :: Expr -> Usingl KExpr
  UFormTy :: FormTy -> Usingl KFormTy
  UCollType :: CollType -> Usingl KCollType
  UTerm :: Term -> Usingl KTerm
  UTag :: Tag -> Usingl KTag

data Constr :: * where
  C1Nil  :: Constr
  C1Singleton  :: Constr
  C1Cons :: Constr

  C2Space :: Constr
  C2Comma :: Constr
  C2NewLine :: Constr

  C3Special  :: Constr
  C3Dispatch  :: Constr
  C3Collection  :: Constr
  C3Term  :: Constr
  C3Comment :: Constr
  C3Seq      :: Constr

  C4Quote :: Constr
  C4SQuote :: Constr
  C4UnQuote :: Constr
  C4SUnQuote :: Constr
  C4DeRef :: Constr

  C5Vec :: Constr
  C5Set :: Constr
  C5Parens :: Constr

  C6TaggedString :: Constr

  C7String :: Constr
  C7Metadata :: Constr
  C7Var :: Constr
  deriving Show

data ConstrFor :: U -> Constr -> * where
  C1NilProof :: ConstrFor KSepExprList C1Nil
  C1SingletonProof :: ConstrFor KSepExprList C1Singleton
  C1ConsProof :: ConstrFor KSepExprList C1Cons

  C2SpaceProof :: ConstrFor KSep C2Space
  C2CommaProof :: ConstrFor KSep C2Comma
  C2NewLineProof :: ConstrFor KSep C2NewLine

  C3SpecialProof :: ConstrFor KExpr C3Special
  C3DispatchProof :: ConstrFor KExpr C3Dispatch
  C3CollectionProof :: ConstrFor KExpr C3Collection
  C3TermProof :: ConstrFor KExpr C3Term
  C3CommentProof :: ConstrFor KExpr C3Comment
  C3SeqProof :: ConstrFor KExpr C3Seq

  C4QuoteProof :: ConstrFor KFormTy C4Quote
  C4SQuoteProof :: ConstrFor KFormTy C4SQuote
  C4UnQuoteProof :: ConstrFor KFormTy C4UnQuote
  C4SUnQuoteProof :: ConstrFor KFormTy C4SUnQuote
  C4DeRefProof :: ConstrFor KFormTy C4DeRef

  C5VecProof :: ConstrFor KCollType C5Vec
  C5SetProof :: ConstrFor KCollType C5Set
  C5ParensProof :: ConstrFor KCollType C5Parens

  C6TaggedStringProof :: ConstrFor KTerm C6TaggedString

  C7StringProof :: ConstrFor KTag C7String
  C7MetadataProof :: ConstrFor KTag C7Metadata
  C7VarProof :: ConstrFor KTag C7Var

deriving instance Show (ConstrFor u c)

showConstr :: ConstrFor u c -> String
showConstr C1NilProof = "Nil "
showConstr C1SingletonProof = "Singleton "
showConstr C1ConsProof = "Cons "

showConstr C2SpaceProof = "Space "
showConstr C2CommaProof = "Comma "
showConstr C2NewLineProof = "NewLine "

showConstr C3SpecialProof = "Special "
showConstr C3DispatchProof = "Dispatch "
showConstr C3CollectionProof = "Collection "
showConstr C3TermProof = "Term "
showConstr C3CommentProof = "Comment "
showConstr C3SeqProof = "Seq "

showConstr C4QuoteProof = "Quote "
showConstr C4SQuoteProof = "SQuote "
showConstr C4UnQuoteProof = "UnQuote "
showConstr C4SUnQuoteProof = "SUnQuote "
showConstr C4DeRefProof = "DeRef "

showConstr C5VecProof = "Vec "
showConstr C5SetProof = "Set "
showConstr C5ParensProof = "Parens "

showConstr C6TaggedStringProof = "TaggedString "

showConstr C7StringProof = "String "
showConstr C7MetadataProof = "Metadata "
showConstr C7VarProof = "Var "

type family TypeOf (c :: Constr) :: [U] where
  TypeOf C1Nil = '[]
  TypeOf C1Singleton = '[KExpr]
  TypeOf C1Cons = '[KExpr, KSep, KSepExprList]

  TypeOf C2Space = '[]
  TypeOf C2Comma = '[]
  TypeOf C2NewLine = '[]

  TypeOf C3Special = '[KFormTy, KExpr]
  TypeOf C3Dispatch = '[KExpr]
  TypeOf C3Collection = '[KCollType, KSepExprList]
  TypeOf C3Term = '[KTerm]
  TypeOf C3Comment = '[KString]
  TypeOf C3Seq = '[KExpr, KExpr]

  TypeOf C4Quote = '[]
  TypeOf C4SQuote = '[]
  TypeOf C4UnQuote = '[]
  TypeOf C4SUnQuote = '[]
  TypeOf C4DeRef = '[]

  TypeOf C5Vec = '[]
  TypeOf C5Set = '[]
  TypeOf C5Parens = '[]

  TypeOf C6TaggedString = '[KTag, KString]

  TypeOf C7String = '[]
  TypeOf C7Metadata = '[]
  TypeOf C7Var = '[]

class IsRecEl (u :: U) where
instance IsRecEl KExpr where
instance IsRecEl KSepExprList where
instance IsRecEl KTerm where
instance IsRecEl KSep where
instance IsRecEl KCollType where
instance IsRecEl KFormTy where
instance IsRecEl KTag where
instance {-# OVERLAPPABLE #-} (TypeError (Text "Not a recursive guy: " :<>: ShowType s))
         => IsRecEl s

-- Library stuff
inj :: ConstrFor r c -> All Usingl (TypeOf c) -> Usingl r
inj C1NilProof An = USepExprList Nil
inj C1SingletonProof (e `Ac` An) = USepExprList (Singleton (eval e))
inj C1ConsProof (e `Ac` sep `Ac` sl `Ac` An) = USepExprList (Cons (eval e) (eval sep) (eval sl))
inj C2SpaceProof An = USep Space
inj C2CommaProof An = USep Comma
inj C2NewLineProof An = USep NewLine
inj C3SpecialProof (fty `Ac` e `Ac` An) = UExpr (Special (eval fty) (eval e))
inj C3DispatchProof (e `Ac` An) = UExpr (Dispatch (eval e))
inj C3CollectionProof (ct `Ac` sl `Ac` An) = UExpr (Collection (eval ct) (eval sl))
inj C3TermProof (t `Ac` An) = UExpr (Term (eval t))
inj C3CommentProof (c `Ac` An) = UExpr (Comment (eval c))
inj C3SeqProof (p `Ac` q `Ac` An) = UExpr (Seq (eval p) (eval q))
inj C4QuoteProof An = UFormTy Quote
inj C4SQuoteProof An = UFormTy SQuote
inj C4UnQuoteProof An = UFormTy UnQuote
inj C4SUnQuoteProof An = UFormTy SUnQuote
inj C4DeRefProof An = UFormTy DeRef
inj C5VecProof An = UCollType Vec
inj C5SetProof An = UCollType Set
inj C5ParensProof An = UCollType Parens
inj C6TaggedStringProof (t `Ac` s `Ac` An) = UTerm (TaggedString (eval t) (eval s))
inj C7StringProof An = UTag String
inj C7MetadataProof An = UTag Metadata
inj C7VarProof An = UTag Var

type family El (u :: U) where
  El KString = String
  El KSep = Sep
  El KSepExprList = SepExprList
  El KExpr = Expr
  El KFormTy = FormTy
  El KCollType = CollType
  El KTerm = Term
  El KTag = Tag

eval :: Usingl u -> El u
eval (UString u) = u
eval (USep u) = u
eval (USepExprList u) = u
eval (UExpr u) = u
eval (UFormTy u) = u
eval (UCollType u) = u
eval (UTerm u) = u
eval (UTag u) = u

instance TestEquality (ConstrFor u) where
  testEquality a b = case testEquality' a b of
    Just (Refl, Refl) -> Just Refl
    Nothing -> Nothing


testEquality' :: ConstrFor a b -> ConstrFor c d -> Maybe ((a :~: c), (b :~: d))
testEquality' C1NilProof C1NilProof = Just (Refl, Refl)
testEquality' C1SingletonProof C1SingletonProof = Just (Refl, Refl)
testEquality' C1ConsProof C1ConsProof = Just (Refl, Refl)
testEquality' C2SpaceProof C2SpaceProof = Just (Refl, Refl)
testEquality' C2CommaProof C2CommaProof = Just (Refl, Refl)
testEquality' C2NewLineProof C2NewLineProof = Just (Refl, Refl)
testEquality' C3SpecialProof C3SpecialProof = Just (Refl, Refl)
testEquality' C3DispatchProof C3DispatchProof = Just (Refl, Refl)
testEquality' C3CollectionProof C3CollectionProof = Just (Refl, Refl)
testEquality' C3TermProof C3TermProof = Just (Refl, Refl)
testEquality' C3CommentProof C3CommentProof = Just (Refl, Refl)
testEquality' C3SeqProof C3SeqProof = Just (Refl, Refl)
testEquality' C4QuoteProof C4QuoteProof = Just (Refl, Refl)
testEquality' C4SQuoteProof C4SQuoteProof = Just (Refl, Refl)
testEquality' C4UnQuoteProof C4UnQuoteProof = Just (Refl, Refl)
testEquality' C4SUnQuoteProof C4SUnQuoteProof = Just (Refl, Refl)
testEquality' C4DeRefProof C4DeRefProof = Just (Refl, Refl)
testEquality' C5VecProof C5VecProof = Just (Refl, Refl)
testEquality' C5SetProof C5SetProof = Just (Refl, Refl)
testEquality' C5ParensProof C5ParensProof = Just (Refl, Refl)
testEquality' C6TaggedStringProof C6TaggedStringProof = Just (Refl, Refl)
testEquality' C7StringProof C7StringProof = Just (Refl, Refl)
testEquality' C7MetadataProof C7MetadataProof = Just (Refl, Refl)
testEquality' C7VarProof C7VarProof = Just (Refl, Refl)
testEquality' _ _ = Nothing



instance TestEquality Usingl where
  testEquality (UString _) (UString _) = Just Refl
  testEquality (USep _) (USep _) = Just Refl
  testEquality (USepExprList _) (USepExprList _) = Just Refl
  testEquality (UExpr _) (UExpr _) = Just Refl
  testEquality (UFormTy _) (UFormTy _) = Just Refl
  testEquality (UCollType _) (UCollType _) = Just Refl
  testEquality (UTerm _) (UTerm _) = Just Refl
  testEquality (UTag _) (UTag _) = Just Refl
  testEquality _ _ = Nothing

instance Eq (Usingl a) where
  (UString a) == (UString b) = a == b
  (USep a) == (USep b) = a == b
  (USepExprList a) == (USepExprList b) = a == b
  (UExpr a) == (UExpr b) = a == b
  (UFormTy a) == (UFormTy b) = a == b
  (UCollType a) == (UCollType b) = a == b
  (UTerm a) == (UTerm b) = a == b
  (UTag a) == (UTag b) = a == b

data View u where
 Tag :: ConstrFor u c -> All Usingl (TypeOf c) -> View u

view :: IsRecEl r => Usingl r -> View r
view (UExpr expr) = viewExpr expr
view (USepExprList sepexprlist) = viewSepExprList sepexprlist
view (USep sep) = viewSep sep
view (UFormTy formty) = viewFormTy formty
view (UCollType colltype) = viewCollType colltype
view (UTerm term) = viewTerm term
view (UTag tag) = viewTag tag

viewSepExprList :: SepExprList -> View KSepExprList
viewSepExprList Nil = Tag C1NilProof An
viewSepExprList (Singleton e) = Tag C1SingletonProof (UExpr e .@. An)
viewSepExprList (Cons e s sl) = Tag C1ConsProof (UExpr e .@. USep s .@. USepExprList sl .@. An)

viewSep :: Sep -> View KSep
viewSep Space = Tag C2SpaceProof An
viewSep Comma = Tag C2CommaProof An
viewSep NewLine = Tag C2NewLineProof An

viewExpr :: Expr -> View KExpr
viewExpr (Special fty e) = Tag C3SpecialProof (UFormTy fty .@. UExpr e .@. An)
viewExpr (Dispatch e) = Tag C3DispatchProof  (UExpr e .@. An)
viewExpr (Collection ct sl) = Tag C3CollectionProof (UCollType ct .@. USepExprList sl .@. An)
viewExpr (Term t) = Tag C3TermProof (UTerm t .@. An)
viewExpr (Comment c) = Tag C3CommentProof (UString c .@. An)
viewExpr (Seq p q)  = Tag C3SeqProof (UExpr p .@. UExpr q .@. An)

viewFormTy :: FormTy -> View KFormTy
viewFormTy Quote = Tag C4QuoteProof An
viewFormTy SQuote = Tag C4SQuoteProof An
viewFormTy UnQuote = Tag C4UnQuoteProof An
viewFormTy SUnQuote = Tag C4SUnQuoteProof An
viewFormTy DeRef = Tag C4DeRefProof An

viewCollType :: CollType -> View KCollType
viewCollType Vec = Tag C5VecProof An
viewCollType Set = Tag C5SetProof An
viewCollType Parens = Tag C5ParensProof An

viewTerm :: Term -> View KTerm
viewTerm (TaggedString t s) = Tag C6TaggedStringProof (UTag t .@. UString s .@. An)

viewTag :: Tag -> View KTag
viewTag String = Tag C7StringProof An
viewTag Metadata = Tag C7MetadataProof An
viewTag Var = Tag C7VarProof An


onRecursiveGuy :: ((IsRecEl v) => Usingl v -> a) -> (Usingl v -> a) -> Usingl v -> a
onRecursiveGuy rec nonrec at@(UString _) = nonrec at
onRecursiveGuy rec nonrec at@(USep _) = rec at
onRecursiveGuy rec nonrec at@(USepExprList _) = rec at
onRecursiveGuy rec nonrec at@(UExpr _) = rec at
onRecursiveGuy rec nonrec at@(UFormTy _) = rec at
onRecursiveGuy rec nonrec at@(UCollType _) = rec at
onRecursiveGuy rec nonrec at@(UTerm _) = rec at
onRecursiveGuy rec nonrec at@(UTag _) = rec at

-- Utility
type family Le (k :: *) :: U where
  Le Sep = KSep
  Le SepExprList = KSepExprList
  Le Expr = KExpr
  Le FormTy = KFormTy
  Le CollType = KCollType
  Le Term = KTerm
  Le Tag = KTag

class Sing a where
  toSing :: a -> Usingl (Le a)

instance Sing Sep where
  toSing Space = USep Space
  toSing Comma = USep Comma
  toSing NewLine = USep NewLine
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
  toSing (Seq p q)   = UExpr (Seq p q)
instance Sing FormTy where
  toSing Quote = UFormTy Quote
  toSing SQuote = UFormTy SQuote
  toSing UnQuote = UFormTy UnQuote
  toSing SUnQuote = UFormTy SUnQuote
  toSing DeRef = UFormTy DeRef
instance Sing CollType where
  toSing Vec = UCollType Vec
  toSing Set = UCollType Set
  toSing Parens = UCollType Parens
instance Sing Term where
  toSing (TaggedString t s) = UTerm (TaggedString t s)
instance Sing Tag where
  toSing String = UTag String
  toSing Metadata = UTag Metadata
  toSing Var = UTag Var

instance Show (Usingl u) where
  show (UString u) = show u
  show (USep u) = show u
  show (USepExprList u) = show u
  show (UExpr u) = show u
  show (UFormTy u) = show u
  show (UCollType u) = show u
  show (UTerm u) = show u
  show (UTag u) = show u

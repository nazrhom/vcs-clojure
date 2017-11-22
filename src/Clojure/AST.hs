{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Clojure.AST where

import Text.Parsec.Pos
import GHC.Generics

data SepExprList =
   Nil LineRange
 | Cons Expr Sep SepExprList LineRange
 deriving (Show, Generic)

type Sep = String
-- data Sep = Space | Comma | NewLine deriving (Show, Eq)

data Expr = Special FormTy Expr LineRange
          | Dispatch Expr LineRange
          | Collection CollType SepExprList LineRange
          | Term Term LineRange
          | Comment String LineRange
          | Seq Expr Expr LineRange
          | Empty LineRange
          deriving (Show, Generic)
-- ref: https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html
type FormTy = String
-- data FormTy = Quote | SQuote | UnQuote | DeRef deriving (Show, Eq)

type CollType = String
-- data CollType = Vec | Set | Parens deriving (Show, Eq)

data Term = TaggedString Tag String LineRange
          deriving (Show, Generic)

type Tag = String
-- data Tag = String | Metadata | Var  deriving (Show, Eq)

data SubTree = Exp Expr | Sel SepExprList
  deriving (Show)

collectSubTrees :: Expr -> Int -> [SubTree]
collectSubTrees e i = mbTakeExpr e i

collectSubTreesSel :: SepExprList -> Int -> [SubTree]
collectSubTreesSel (Nil lr) i = []
collectSubTreesSel (Cons e _ sel _) i = collectSubTreesExpr e i ++ mbTakeSel sel i

mbTakeExpr :: Expr -> Int -> [SubTree]
mbTakeExpr e i = if (takeStart $ extractRangeExpr e) == i
  then (Exp e):collectSubTreesExpr e i
  else collectSubTreesExpr e i

mbTakeSel :: SepExprList -> Int -> [SubTree]
mbTakeSel sel@(Cons _ _ _ lr) i = if (takeStart lr) == i
  then (Sel sel):collectSubTreesSel sel i
  else collectSubTreesSel sel i
mbTakeSel sel i = collectSubTreesSel sel i

collectSubTreesExpr :: Expr -> Int -> [SubTree]
collectSubTreesExpr (Collection _ sel _) i = mbTakeSel sel i
collectSubTreesExpr (Seq e1 e2 _) i = collectSubTreesExpr e1 i ++ collectSubTrees e2 i
collectSubTreesExpr (Special _ e _) i = collectSubTreesExpr e i
collectSubTreesExpr (Dispatch e _) i = collectSubTreesExpr e i
collectSubTreesExpr _ i = []

data ConflictResult a = NoConflict | ConflictAt a
  deriving (Show, Eq)

emptyRange :: LineRange
emptyRange = Range 0 0

data LineRange = Range Int Int
  deriving (Show, Eq)

mkRange :: SourcePos -> SourcePos -> LineRange
mkRange s e = Range (sourceLine s) (sourceLine e)

takeStart :: LineRange -> Int
takeStart (Range s e) = s

takeEnd :: LineRange -> Int
takeEnd (Range s e) = e

extractRangeExpr :: Expr -> LineRange
extractRangeExpr (Special _ _ r) = r
extractRangeExpr (Dispatch _ r) = r
extractRangeExpr (Collection _ _ r) = r
extractRangeExpr (Term _ r) = r
extractRangeExpr (Comment _ r) = r
extractRangeExpr (Seq _ _ r) = r
extractRangeExpr (Empty r) = r

extractRangeSepExprList :: SepExprList -> LineRange
extractRangeSepExprList (Nil r) = r
extractRangeSepExprList (Cons _ _ _ r) = r

extractRangeTerm :: Term -> LineRange
extractRangeTerm (TaggedString _ _ r) = r

instance Eq Expr where
  (Special fty1 expr1 _) == (Special fty2 expr2 _) = fty1 == fty2 && expr1 == expr2
  (Dispatch e1 _) == (Dispatch e2 _) = e1 == e2
  (Collection ct1 sel1 _) == (Collection ct2 sel2 _) = ct1 == ct2 && sel1 == sel2
  (Term t1 _) == (Term t2 _) = t1 == t2
  (Comment c1 _) == (Comment c2 _) = c1 == c2
  (Seq e1 e2 _) == (Seq e3 e4 _) = e1 == e3 && e2 == e4
  (Empty _) == (Empty _) = True
  _ == _ = False

instance Eq SepExprList where
  (Nil _) == (Nil _) = True
  (Cons e1 s1 r1 _) == (Cons e2 s2 r2 _) = e1 == e2 && s1 == s2 && r1 == r2
  _ == _ = False

instance Eq Term where
  (TaggedString t1 s1 _) == (TaggedString t2 s2 _) = t1 == t2 && s1 == s2

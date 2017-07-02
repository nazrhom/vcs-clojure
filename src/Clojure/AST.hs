module Clojure.AST where

import Text.Parsec.Pos

data SepExprList = Nil | Singleton Expr | Cons Expr Sep SepExprList deriving (Show, Eq)

data Sep = Space | Comma | NewLine deriving (Show, Eq)

data Expr = Special FormTy Expr LineRange
          | Dispatch Expr LineRange
          | Collection CollType SepExprList LineRange
          | Term Term LineRange
          | Comment String LineRange
          | Seq Expr Expr
          deriving (Show)

-- ref: https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html
data FormTy = Quote | SQuote | UnQuote | DeRef deriving (Show, Eq)

data CollType = Vec | Set | Parens deriving (Show, Eq)

data Term = TaggedString Tag String
          deriving (Show, Eq)

data Tag = String | Metadata | Var  deriving (Show, Eq)

emptyRange :: LineRange
emptyRange = Range 0 0

data LineRange = Range Int Int
  deriving (Show, Eq)

mkRange :: SourcePos -> SourcePos -> LineRange
mkRange s e = Range (sourceLine s) (sourceLine e)


instance Eq Expr where
  (Special fty1 expr1 _) == (Special fty2 expr2 _) = fty1 == fty2 && expr1 == expr2
  (Dispatch e1 _) == (Dispatch e2 _) = e1 == e2
  (Collection ct1 sel1 _) == (Collection ct2 sel2 _) = ct1 == ct2 && sel1 == sel2
  (Term t1 _) == (Term t2 _) = t1 == t2
  (Comment c1 _) == (Comment c2 _) = c1 == c2
  (Seq e1 e2) == (Seq e3 e4) = e1 == e3 && e2 == e4
  _ == _ = False

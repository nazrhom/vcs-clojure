module Clojure.AST where


data SepExprList = Nil | Singleton Expr | Cons Expr Sep SepExprList deriving (Show, Eq)

data Sep = Space | Comma | NewLine deriving (Show, Eq)

data Expr = Special FormTy Expr
          | Dispatch Expr
          | Collection CollType (SepExprList)
          | Term Term
          | Comment String
          | Seq Expr Expr
          deriving (Show, Eq)

-- ref: https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html
data FormTy = Quote | SQuote | UnQuote | SUnQuote | DeRef deriving (Show, Eq)

data CollType = Vec | Set | Parens deriving (Show, Eq)

data Term = TaggedString Tag String
          deriving (Show, Eq)

data Tag = String | Metadata | Var  deriving (Show, Eq)

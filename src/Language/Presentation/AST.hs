module Language.Presentation.AST where

data T = Sum T T | Atom A deriving (Show,Eq)
data A = Val Int | Parens T  deriving (Show,Eq)

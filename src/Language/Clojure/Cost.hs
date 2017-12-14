{-# LANGUAGE GADTs #-}

module Language.Clojure.Cost where

import Language.Clojure.AST
import Language.Clojure.Lang

costUsingl :: Usingl u -> Int
costUsingl (UString u) = 1
costUsingl (USep u) = 1
costUsingl (UCollTy u) = 1
costUsingl (UFormTy u) = 1
costUsingl (UTag u) = 1
costUsingl (UExpr e) = costExpr e
costUsingl (USepExprList sel) = costSepExprist sel
costUsingl (UTerm t) = costTerm t

costExpr :: Expr -> Int
costExpr (Special fty e _) = 1 + costExpr e
costExpr (Dispatch e _) = costExpr e
costExpr (Collection cty sel _) = 1 + costSepExprist sel
costExpr (Term t _) = costTerm t
costExpr (Comment s _) = 1
costExpr (Seq e1 e2 _) = costExpr e1 + costExpr e2
costExpr (Empty _) = 0

costSepExprist :: SepExprList -> Int
costSepExprist (Nil _) = 0
costSepExprist (Cons e sep sel _) = 1 + costExpr e + costSepExprist sel

costTerm :: Term -> Int
costTerm t = 2
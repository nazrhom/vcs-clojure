{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module Language.Clojure.Cost where

import Language.Clojure.AST
import Language.Clojure.Lang

costUsingl :: Usingl u -> Int
{-# INLINE costUsingl #-}
costUsingl (UString u) = 1
costUsingl (USep u) = 1
costUsingl (UCollTy u) = 1
costUsingl (UFormTy u) = 1
costUsingl (UTag u) = 1
costUsingl (UExpr e) = costExpr e
costUsingl (USepExprList sel) = costSepExprList sel
costUsingl (UTerm t) = 2

costExpr :: Expr -> Int
{-# INLINE costExpr #-}
costExpr (Special fty e _) = 2 + costExpr e
costExpr (Dispatch e _) = 1 + costExpr e
costExpr (Collection cty sel _) = 2 + costSepExprList sel
costExpr (Term t _) = 3
costExpr (Comment s _) = 2
costExpr (Seq e1 e2 _) = 1 + costExpr e1 + costExpr e2
costExpr (Empty _) = 1

costSepExprList :: SepExprList -> Int
{-# INLINE costSepExprList #-}
costSepExprList (Nil _) = 1
costSepExprList (Cons e sep sel _) = 2 + costExpr e + costSepExprList sel


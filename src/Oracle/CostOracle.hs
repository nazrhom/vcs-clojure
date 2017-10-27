{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.CostOracle where

import Control.Monad.State

import Oracle.Internal

import Clojure.Lang
import Clojure.AST
import Clojure.Parser

data CostOracle = CostOracle (State Int ())


-- instance (Monad m) => OracleF DiffOracle m where
--   callF o s d = do
--
-- instance (Monad m) => OracleP DiffOracle m where
--   callP _ An         An         = return []
--   callP _ An         (_ `Ac` _) = return [ I ]
--   callP _ (_ `Ac` _) An         = return [ D ]
--   callP o (s `Ac` _) (d `Ac` _) = []

inSync :: LineRange -> LineRange -> Bool
inSync (Range s1 _) (Range s2 _) = s1 == s2

isContainedIn :: LineRange -> LineRange -> Bool
isContainedIn (Range s1 e1) (Range s2 e2) = s2 <= s1 && e1 <= e2
--
-- calculateMaxCost :: [GroupDiffAction] -> Expr -> Expr -> Int
-- calculateMaxCost actions src dst = undefined
--
extractExprFromRange :: Expr -> LineRange -> Expr
extractExprFromRange expr range = if range `inSync` (extractRangeExpr expr)
  then expr
  else case expr of
    (Special _ e _) -> extractExprFromRange e range
    (Dispatch e _) -> extractExprFromRange e range
    (Collection cty sel lr) -> Collection cty (extractSepExprListFromRange sel range) lr
    (Term t _) -> expr
    (Comment s _) -> expr
    (Seq e1 e2 lr) -> if not (range `isContainedIn` (extractRangeExpr e1))
      then extractExprFromRange e2 range
      else if not (range `isContainedIn` (extractRangeExpr e2))
      then extractExprFromRange e1 range
      else (Seq (extractExprFromRange e1 range) (extractExprFromRange e2 range) lr)

extractSepExprListFromRange :: SepExprList -> LineRange -> SepExprList
extractSepExprListFromRange expr@(Cons e sep sel _) range =
  if not (range `isContainedIn` (extractRangeExpr e))
    then extractSepExprListFromRange sel range
  else if not (range `isContainedIn` (extractRangeSepExprList sel)) then (Singleton e emptyRange)
  else expr
extractSepExprListFromRange expr _ = expr


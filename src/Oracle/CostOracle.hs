{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.CostOracle where

import Control.Monad.State

import Oracle.Internal

import Clojure.Lang
import Clojure.AST
import Clojure.Parser

data SeqOracle = SeqOracle

instance (Monad m) => OracleF SeqOracle m where
  callF o s d = return $ askOracle s d

instance (Monad m) => OracleP SeqOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [ I ]
  callP _ (_ `Ac` _) An         = return [ D ]
  callP o (s `Ac` _) (d `Ac` _) = return $ askOracle s d

askOracle :: Usingl u -> Usingl v -> [Path]
askOracle (UExpr e1) (UExpr e2) =
  if isSeq e1 && isSeq e2 then [ M ]
  else if isSeq e1 then [ D ]
  else if isSeq e2 then [ I ]
  else []
askOracle _ _ = []

isSeq :: Expr -> Bool
isSeq (Seq _ _ _) = True
isSeq _ = False
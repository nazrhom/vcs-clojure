{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.ConsOracle where

import Control.Monad.Reader

import Oracle.Internal
import Clojure.Lang
import Clojure.AST

data ConsOracle = ConsOracle
instance (Monad m) => OracleP ConsOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [I]
  callP _ (_ `Ac` _) An         = return [D]
  callP _ (u `Ac` _) (v `Ac` _) = do
    p <- ask
    return $ skipCons u v p

instance (Monad m) => OracleF ConsOracle m where
  callF _ u v = do
    p <- ask
    return $ skipCons u v p


skipCons :: Usingl u -> Usingl v -> [Path] -> [Path]
skipCons (USepExprList (Cons _ _ _ _)) (USepExprList (Cons _ _ _ _)) (x:xs) = [x]
skipCons _ _ _ = []

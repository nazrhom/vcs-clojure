{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Oracle.Internal where

import Control.Monad.Reader
import Control.Applicative
import Data.List

import Clojure.Lang
import Clojure.AST

data Path
  = I | M | D
  deriving (Eq , Show)

-- * The history is the list of all issued instructions.
--   XXX: DO NOT CONFUSE WITH THE LIST OF POSSIBLE PATHS
--        TO PURSUE
type History = [Path]

-- * The Oracle will have access to it's previously issued history
--   ON THE CURRENT BRANCH.
--
type HistoryM = ReaderT History

-- * Oracle for alignment
class (Monad m) => OracleP o m where
  callP :: o -> All Usingl p1 -> All Usingl p2 -> HistoryM m [Path]

class (Monad m) => OracleF o m where
  callF :: (IsRecEl u, IsRecEl v) => o -> Usingl u -> Usingl v -> HistoryM m [Path]

type MonadOracle o m = (Alternative m , OracleP o m , OracleF o m)

data ComposeOracle a b = ComposeOracle a b

(<°>) :: a -> b -> ComposeOracle a b
a <°> b = ComposeOracle a b

instance (Monad m, OracleF a m, OracleF b m) => OracleF (ComposeOracle a b) m where
  callF (ComposeOracle a b) s d = do
    o1 <- callF a s d
    o2 <- callF b s d
    return $ intersect o1 o2

instance (Monad m, OracleP a m, OracleP b m) => OracleP (ComposeOracle a b) m where
  callP (ComposeOracle a b) s d = do
    o1 <- callP a s d
    o2 <- callP b s d
    return $ intersect o1 o2

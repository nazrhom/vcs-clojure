{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Oracle.Internal where

import Control.Monad.Reader
import Control.Applicative
import Data.List
import Data.Type.Equality

import Debug.Trace

import Clojure.Lang
import Clojure.AST

data Path
  = I | M | D | FM
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
data PickBest a b = PickBest a b

(<°>) :: a -> b -> ComposeOracle a b
a <°> b = ComposeOracle a b

(<+>) :: a -> b -> PickBest a b
a <+> b = PickBest a b

instance (Monad m, OracleF a m, OracleF b m) => OracleF (ComposeOracle a b) m where
  callF (ComposeOracle a b) s d = do
    o1 <- callF a s d
    case o1 of
      [] -> do
        -- traceM ("Calling o2 on.\nsrc:" ++ show (extractRange s) ++ "\ndst:" ++ show (extractRange d))
        callF b s d
      o1 -> do
        -- traceM ("Calling o1 on.\nsrc:" ++ show (extractRange s) ++ "\ndst:" ++ show (extractRange d))
        return o1

instance (Monad m, OracleP a m, OracleP b m) => OracleP (ComposeOracle a b) m where
  callP (ComposeOracle a b) s d = do
    o1 <- callP a s d
    case o1 of
      [] -> callP b s d
      o1 -> return o1

instance (Monad m, OracleP a m, OracleP b m) => OracleP (PickBest a b) m where
  callP (PickBest a b) s d = do
    o1 <- callP a s d
    o2 <- callP b s d
    if (length o1 <= length o2 && length o1 > 0)
    then return o1
    else return o2

instance (Monad m, OracleF a m, OracleF b m) => OracleF (PickBest a b) m where
  callF (PickBest a b) s d = do
    o1 <- callF a s d
    o2 <- callF b s d
    if (length o1 <= length o2 && length o1 > 0)
    then return o1
    else return o2

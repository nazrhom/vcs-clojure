{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Oracle.Internal where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Applicative
import Data.List
import Data.Type.Equality

import Debug.Trace

import Language.Common
import Language.Clojure.Lang
import Language.Clojure.AST

data Path
  = I | M | D | FM | S
  deriving (Eq , Show)

-- * The history is the list of all issued instructions.
--   XXX: DO NOT CONFUSE WITH THE LIST OF POSSIBLE PATHS
--        TO PURSUE
type History = [Path]


-- data History = History {
--     path :: [Path]
--   , cost :: Int
--   } deriving (Show)

initialHistory :: History
initialHistory = [I,M,D]
--   History {
--     path = [I,M,D]
--   , cost = 0
-- }

liftH :: (Monad m) => m a -> HistoryM m a
{-# INLINE liftH #-}
liftH = HistoryM . (lift . lift)

getCurrentCost :: (Monad m) => HistoryM m Int
{-# INLINE getCurrentCost #-}
getCurrentCost = get

updateCost :: (Monad m) => Int -> HistoryM m Int
{-# INLINE updateCost #-}
updateCost i = do
  c <- get
  put (i + c)
  return $ i+c

-- * The Oracle will have access to it's previously issued history
--   ON THE CURRENT BRANCH.
--
newtype HistoryM m a = HistoryM {
    runH :: ReaderT History (StateT Int m) a
} deriving (Monad, MonadReader History, MonadState Int, Applicative, Functor, Alternative)

runHistory :: (Monad m) => History -> Int -> HistoryM m a -> m (a, Int)
runHistory h i k = runStateT (runReaderT (runH k) h) i

evalHistory :: (Monad m) => History -> Int -> HistoryM m a -> m a
evalHistory h i k = evalStateT (runReaderT (runH k) h) i
-- * Oracle for alignment
class Oracle o m where
  callOP :: o -> All Usingl p1 -> All Usingl p2 -> HistoryM m [Path]
  callOF :: (IsRecEl u, IsRecEl v) => o -> Usingl u -> Usingl v -> HistoryM m [Path]

class (Monad m) => OracleP o m where
  callP :: o -> All Usingl p1 -> All Usingl p2 -> HistoryM m [Path]

class (Monad m) => OracleF o m where
  callF :: (IsRecEl u, IsRecEl v) => o -> Usingl u -> Usingl v -> HistoryM m [Path]

instance (OracleF o m, OracleP o m) => Oracle o m where
  callOP = callP
  callOF = callF

type MonadOracle o m = (MonadPlus m, OracleP o m , OracleF o m)

data ComposeOracle a b = ComposeOracle a b
data PickBest a b = PickBest a b

(<°>) :: a -> b -> ComposeOracle a b
a <°> b = ComposeOracle a b

(<+>) :: a -> b -> PickBest a b
a <+> b = PickBest a b

instance (Monad m, OracleF a m, OracleF b m) => OracleF (ComposeOracle a b) m where
  callF (ComposeOracle a b) s d = do
    -- traceM "Calling o1"
    o1 <- callF a s d
    case o1 of
      [] -> do
        -- traceM "Calling o2"
        callF b s d
      o1 -> do
        return o1

instance (Monad m, OracleP a m, OracleP b m) => OracleP (ComposeOracle a b) m where
  callP (ComposeOracle a b) s d = do
    -- traceM "calling o1"
    o1 <- callP a s d
    case o1 of
      [] -> do
        -- traceM "calling o2"
        callP b s d
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

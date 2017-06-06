{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Oracle where

import Control.Applicative
import Data.Type.Equality hiding (apply)
import Control.Monad.Reader

import Lang

data Path
  = I | M | D
  deriving (Eq , Show)

-- * The history is the list of all issued instructions.
--   XXX: DO NOT CONFUSE WITH THE LIST OF POSSIBLE PATHS
--        TO PORSUE
type History = [Path]

-- * The Oracle will have access to it's previously issued history
--   ON THE CURRENT BRANCH.
--
type HistoryM = ReaderT History

-- * Oracle for alignment
class (Monad m) => OracleP o m where
  callP :: o -> All Usingl p1 -> All Usingl p2 -> HistoryM m [Path]

class (Monad m) => OracleF o m where
  callF :: o -> Usingl u -> Usingl v -> HistoryM m [Path]

type MonadOracle o m = (Alternative m , OracleP o m)

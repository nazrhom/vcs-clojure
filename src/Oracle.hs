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

type MonadOracle o m = (Alternative m , OracleP o m , OracleF o m)

-- XXX: Every Oracle should go in its own module

-- * The "dumb" oracle, then, is:
data NoOracle = NoOracle
instance (Monad m) => OracleP NoOracle m where
  callP _ _ _ = return [I , M , D]
instance (Monad m) => OracleF NoOracle m where
  callF _ _ _ = return [I , M , D]
  
data NoDupBranches = NoDubBranches

nextPaths :: [Paths] -> [Paths]
nextPaths []    = [I , M , D]
nextPaths (I:_) = [I , M]
nextPaths (D:_) = [D , M]
nextPaths (M:_) = [I , M , D]

instance (Monad m) => OracleP NoDupBranches where
  callP _ An An                 = return []
  callP _ (_ `Ac` _) (_ `Ac` _) = ask >>= return . nextPaths
  -- XXX: finish!

instance (Monad m) => OracleF NoDubBranches where
  callF = undefined

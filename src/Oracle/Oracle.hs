{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}


module Oracle.Oracle where

import Control.Applicative
import Data.Type.Equality hiding (apply)
import Control.Monad.Reader
import qualified Data.IntMap.Strict as M
import Data.Maybe
import Data.List

import Clojure.Lang
import Clojure.AST
import Util.UnixDiff

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

-- XXX: Every Oracle should go in its own module

-- * The "dumb" oracle, then, is:
data NoOracle = NoOracle
instance (Monad m) => OracleP NoOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [I]
  callP _ (_ `Ac` _) An         = return [D]
  callP _ _          _          = return [I , M , D]

instance (Monad m) => OracleF NoOracle m where
  callF _ _ _ = return [I , M , D]

data NoDupBranches = NoDupBranches

nextPaths :: [Path] -> [Path]
nextPaths []        = [I , M , D]
nextPaths (I:_)     = [I , M]
nextPaths (D:_)     = [D , M]
nextPaths (M:_)     = [I , M , D]

instance (Monad m) => OracleP NoDupBranches m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return []
  callP _ (_ `Ac` _) An         = return [D]
  callP _ (_ `Ac` _) (_ `Ac` _) = ask >>= return . nextPaths

instance (Monad m) => OracleF NoDupBranches m where
  callF _ _ _  = ask >>= return . nextPaths

data DiffOracle = DiffOracle (M.IntMap DiffAction)

buildOracle :: [DiffResult] -> M.IntMap DiffAction
buildOracle [] = M.empty
buildOracle ((DiffResult a i):rest) = M.insert i a (buildOracle rest)

askOracle :: LineRange -> DiffOracle -> Maybe DiffAction
askOracle (Range start end) (DiffOracle m) = follow (start+1) end (fromJust $ M.lookup start m)
  where
    follow :: Int -> Int -> DiffAction -> Maybe DiffAction
    follow s e a | s >= e = if findInM e a then Just a else Nothing
    follow s e a | otherwise = if findInM s a then follow (s+1) e a else Nothing

    findInM i a = (fromJust $ M.lookup i m) == a

instance (Monad m) => OracleF DiffOracle m where
  callF o s d = case askOracle (fromJust $ extractRange s) o of
    Nothing -> return [I, M, D]
    Just a -> return [convert a]

instance (Monad m) => OracleP DiffOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [I]
  callP _ (_ `Ac` _) An         = return [D]
  callP o (s `Ac` _) (_ `Ac` _) = case extractRange s of
    Nothing -> return [I, M, D]
    Just r  -> case askOracle r o of
      Nothing -> return [I, M, D]
      Just a  -> return [convert a]

convert :: DiffAction -> Path
convert Mod = M
convert Ins = I
convert Del = D

data ComposeOracle a b = ComposeOracle a b

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

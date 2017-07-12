{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.DiffOracle where

import qualified Data.IntMap as M
import Data.Maybe

import Oracle.Internal
import Clojure.Lang
import Clojure.AST
import Util.UnixDiff

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

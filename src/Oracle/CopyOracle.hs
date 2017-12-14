{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.CopyOracle where

import Oracle.Internal

import Util.UnixDiff
import Language.Clojure.Lang
import Language.Clojure.AST
import Language.Clojure.Parser
import Language.Common
import Data.Tuple
import Data.Maybe

import qualified Data.IntMap as M

data CopyOracle = CopyOracle (M.IntMap Int, M.IntMap Int)

buildCopyMap :: [DiffAction] -> M.IntMap Int
buildCopyMap [] = (M.empty)
buildCopyMap (first:rest) = (process first) `M.union` (buildCopyMap rest)
  where
    process (Copy (i1, i2)) = M.singleton i1 i2
    process _ = M.empty

reverseMap :: M.IntMap Int -> M.IntMap Int
reverseMap = M.fromList . map swap . M.toList

buildCopyOracle :: [DiffAction] -> (M.IntMap Int, M.IntMap Int)
buildCopyOracle as = (insMap, reverseMap insMap)
  where
    insMap = buildCopyMap as

rangeIsCopy :: M.IntMap Int -> LineRange -> Bool
rangeIsCopy m (Range start end) = go m start
  where
    go m i | i <= end =
      if isJust (M.lookup i m)
      then go m (i+1)
      else False
    go m i | otherwise = True

rangeContainsCopy :: M.IntMap Int -> LineRange -> Bool
rangeContainsCopy m (Range start end) = go m start
  where
    go m i | i <= end =
      if isJust (M.lookup i m)
      then True
      else go m (i+1)
    go m i | otherwise = False

srcContainsCopy (m, _) r = rangeContainsCopy m r
dstContainsCopy (_, m) r = rangeContainsCopy m r

askOracle :: CopyOracle -> Usingl u -> Usingl v -> [Path]
askOracle (CopyOracle o) src dst = case (extractRange src, extractRange dst) of
      (Nothing, Nothing)         -> [ M ]
      (Just sRange, Nothing)     -> [ ]
      (Nothing, Just dRange)     -> [ ]
      (Just sRange, Just dRange) -> giveAdvice o src dst

instance (Monad m) => OracleF CopyOracle m where
  callF o s d = return $ askOracle o s d

instance (Monad m) => OracleP CopyOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [ I ]
  callP _ (_ `Ac` _) An         = return [ D ]
  callP o (s `Ac` _) (d `Ac` _) = return $ askOracle o s d

giveAdvice :: (M.IntMap Int, M.IntMap Int) -> Usingl u -> Usingl v -> [Path]
giveAdvice o src dst = if srcContainsCopy o srcRange || dstContainsCopy o dstRange
  then [ M ]
  else
    [ ]
  where
    srcRange = fromJust $ extractRange src
    dstRange = fromJust $ extractRange dst
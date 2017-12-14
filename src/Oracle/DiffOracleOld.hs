{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.DiffOracleOld where

import qualified Data.IntMap as M
import Data.Maybe
import Data.List

import Debug.Trace

import Oracle.Internal
import Language.Clojure.Lang
import Language.Clojure.AST
import Util.UnixDiff

type DiffOp = Path
data DiffOracle = DiffOracle DelInsMap
type DelInsMap = (M.IntMap DiffOp, M.IntMap DiffOp)

unionDelInsMap :: DelInsMap -> DelInsMap -> DelInsMap
unionDelInsMap (s1, d1) (s2, d2) = (M.union s1 s2, M.union d1 d2)

buildOracle :: [DiffAction] -> DelInsMap
buildOracle [] = (M.empty, M.empty)
buildOracle (first:rest) = (process first) `unionDelInsMap` (buildOracle rest)
  where
    process (Copy (i1, i2)) = (M.empty, M.empty)
    process (Ins i) = (M.empty, M.singleton i I)
    process (Del i) = (M.singleton i D, M.empty)


askOracle :: DiffOracle -> LineRange -> LineRange -> [Path]
askOracle (DiffOracle (delMap, insMap)) srcRange dstRange =
  if containsRange delMap srcRange
    && containsRange insMap dstRange
    && inSync srcRange dstRange
      then []
  else if containsRange delMap srcRange
       && not (inSync srcRange dstRange)
        then [ D ]
  else if containsRange insMap dstRange
       && not (inSync srcRange dstRange)
        then [ I ]
  else [ M ]
      -- dstSpan = findSpan insMap dstRange
      -- srcSpan = findSpan delMap srcRange
      -- dstOffset = calculateOffset (delMap, insMap) dstStart
      -- srcOffset = calculateOffset (delMap, insMap) srcStart
      -- dstSpan = (Range (dstStart + dstOffset) (dstEnd + dstOffset))
      -- srcSpan = (Range (srcStart - srcOffset) (srcEnd - srcOffset))

inSync :: LineRange -> LineRange -> Bool
inSync (Range s1 _) (Range s2 _) = s1 == s2

findSpan :: M.IntMap DiffOp -> LineRange -> LineRange
findSpan m (Range start end) = go m start end
  where
    go m s e | isJust (M.lookup s m) = go m (s-1) e
    go m s e | isJust (M.lookup e m) = go m s (e + 1)
    go m s e | otherwise = Range (s+1) (e-1)

calculateOffset :: DelInsMap -> Int -> Int
calculateOffset (del, ins) i = process (M.elems splitIns ++ M.elems splitDel)
  where
    (splitIns, _) = M.split (i+1) ins
    (splitDel, _) = M.split (i+1) del
    process [] = 0
    process (I:xs) = (- 1) + process xs
    process (D:xs) = 1 + process xs

intersectsRange :: M.IntMap DiffOp -> LineRange -> Bool
intersectsRange m (Range start end) = go m start
  where
    go m i | i <= end =
      if isJust (M.lookup i m)
      then True
      else go m (i+1)
    go m i | otherwise = False

containsRange :: M.IntMap DiffOp -> LineRange -> Bool
containsRange m (Range start end) = go m start
  where
    go m i | i <= end =
      if isJust (M.lookup i m)
      then go m (i+1)
      else False
    go m i | otherwise = True

instance (Monad m) => OracleF DiffOracle m where
  callF o s d = do
    -- traceM ("src[" ++ show (fromJust $ extractRange s) ++ "]: " ++ show s)
    -- traceM ("dst[" ++ show (fromJust $ extractRange d) ++ "]: " ++ show d)
    let ans = askOracle o (fromJust $ extractRange s) (fromJust $ extractRange d)
    -- traceM ("ans: " ++ show ans)
    return ans

instance (Monad m) => OracleP DiffOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [ I ]
  callP _ (_ `Ac` _) An         = return [ D ]
  callP o (s `Ac` _) (d `Ac` _) = do
    case (extractRange s, extractRange d) of
      (Nothing, Nothing)         -> do
        -- traceM "ans: M"
        return [ M ]
      (Just sRange, Nothing)     -> do
        -- traceM "ans: D"
        return [ D ]
      (Nothing, Just dRange)     -> do
        -- traceM "ans: I"
        return [ I ]
      (Just sRange, Just dRange) -> do
        let ans = askOracle o sRange dRange
        -- traceM ("ans: " ++ show ans)
        return ans

instance Show DiffOracle where
  show (DiffOracle m) = show m

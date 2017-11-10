{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Oracle.GroupedDiffOracle
  ( GroupedDiffOracle(..)
  )
  where

import Data.Maybe

import Debug.Trace

import Control.Monad.Reader
import Data.IntMap as M
import Data.Set as S

import Oracle.Internal
import Clojure.Lang
import Clojure.AST
import Util.UnixDiff

type CopyMap = M.IntMap Int
type CopyMaps = (CopyMap, CopyMap)
data GroupedDiffOracle = GroupedDiffOracle ([GroupDiffAction], CopyMaps)

askOracle :: GroupedDiffOracle -> Usingl u -> Usingl v -> [Path]
askOracle (GroupedDiffOracle (diffActions, copyMaps)) src dst = case (extractRange src, extractRange dst) of
      (Nothing, Nothing)         -> [ M ]
      (Just sRange, Nothing)     -> [ ]
      (Nothing, Just dRange)     -> [ ]
      (Just sRange, Just dRange) -> giveAdvice diffActions src dst

        -- traceM ("src[" ++ show sRange ++ "]: " ++ show s)
        -- traceM ("dst[" ++ show dRange ++ "]: " ++ show d)
        -- traceM ("ans: " ++ show ans)

giveAdvice :: [GroupDiffAction] -> Usingl u -> Usingl v -> [Path]
giveAdvice [] src dst = [ M ]
  where
    dstRange = fromJust $ extractRange dst
    srcRange = fromJust $ extractRange src
giveAdvice ((OMod srcLr dstLr):os) src dst =
  if src `shouldMod` srcLr && dst `shouldMod` dstLr then []
  else if src `shouldMod` srcLr then
    [ D ]
  else if dst `shouldMod` dstLr then
    [ I ]
  else giveAdvice os src dst
  where
    dstRange = fromJust $ extractRange dst
    srcRange = fromJust $ extractRange src

giveAdvice ((OIns lr i):os) src dst =
  if dst `shouldMod` lr
  then [ I ]
  else giveAdvice os src dst
  where
    dstRange = fromJust $ extractRange dst

giveAdvice ((ODel lr i):os) src dst =
  if src `shouldMod` lr
  then [ D ]
  else giveAdvice os src dst
  where
    srcRange = fromJust $ extractRange src

tryDirection :: LineRange -> LineRange -> [Path]
tryDirection srcRange dstRange =
  if (rangeSpan srcRange) > (rangeSpan dstRange) then [ I ]
  else if (rangeSpan srcRange) < (rangeSpan dstRange) then [ D ]
  else []

rangeSpan :: LineRange -> Int
rangeSpan (Range s e) = e - s

inSync :: LineRange -> LineRange -> Bool
inSync (Range s1 _) (Range s2 _) = s1 == s2

isContainedIn :: LineRange -> LineRange -> Bool
isContainedIn (Range s1 e1) (Range s2 e2) = s2 <= s1 && s1 <= e2

isStrictlyContainedIn :: LineRange -> LineRange -> Bool
isStrictlyContainedIn (Range s1 e1) (Range s2 e2) = s2 <= s1 && e1 <= e2
-- && s1 <= e2-- && e1 <= e2

shouldMod :: Usingl u -> LineRange -> Bool
shouldMod u lr = uRange `isContainedIn` lr
-- || any (flip isContainedIn lr) children
  where
    uRange = fromJust $ extractRange u
    children = extractChildRanges u

copySetExpr :: CopyMap -> Expr -> S.Set Int
copySetExpr copyMap e = collectAll copyMap eRange
  where
    eRange = extractRangeExpr e

copySetSEL :: CopyMap -> SepExprList -> S.Set Int
copySetSEL copyMap e = collectAll copyMap eRange
  where
    eRange = extractRangeSepExprList e

collectAll :: CopyMap -> LineRange -> S.Set Int
collectAll cpM (Range s e) = go cpM s
  where
    go m i | i <= e = mbTakeLine m i `S.union` go m (i+1)
    go m i | otherwise = S.empty

    mbTakeLine m i = if isJust (M.lookup i m)
      then S.singleton i
      else S.empty

extractSingleLines :: [LineRange] -> S.Set Int
extractSingleLines [] = S.empty
extractSingleLines ((Range s e):lrs) | s == e = S.singleton s `S.union` extractSingleLines lrs
                                   | otherwise = extractSingleLines lrs

lookupSet :: S.Set Int -> CopyMap -> S.Set Int
lookupSet s cp = S.map (fromJust . flip M.lookup cp) s

intersects :: Ord a => S.Set a -> S.Set a -> Bool
intersects a b = not (S.null (a `S.intersection` b))

checkOverlap target a b = target `intersects` (a `S.difference` overlapping) && target `intersects` (b `S.difference` overlapping)
  where
    overlapping  = a `S.intersection` b
deOptimize :: CopyMaps -> Usingl u -> Usingl v -> Bool
deOptimize (srcMap, dstMap) s@(UExpr (Seq a b lrs)) t@(UExpr (Seq c d lrt)) = inSync lrs lrt && (checkOverlap copyTargetA copySetC copySetD || checkOverlap copyTargetC copySetA copySetB)
  where
    copySetA = copySetExpr srcMap a
    copySetB = copySetExpr srcMap b
    copyTargetA = lookupSet copySetA srcMap
    copySetC = copySetExpr dstMap c
    copySetD = copySetExpr dstMap d
    copyTargetC = lookupSet copySetC dstMap
deOptimize (srcMap, dstMap) s@(USepExprList (Cons a _ b lrs)) t@(USepExprList (Cons c _ d lrt)) = inSync lrs lrt && (checkOverlap copyTargetA copySetC copySetD || checkOverlap copyTargetC copySetA copySetB)
  where
    copySetA = copySetExpr srcMap a
    copySetB = copySetSEL srcMap b
    copyTargetA = lookupSet copySetA srcMap
    copySetC = copySetExpr dstMap c
    copySetD = copySetSEL dstMap d
    copyTargetC = lookupSet copySetC dstMap
deOptimize _ _ _ = False

extractChildRanges :: Usingl u -> [LineRange]
extractChildRanges (UString u) = []
extractChildRanges (UExpr u)   = extractChildExprRange u
extractChildRanges (USepExprList u) = extractChildSepExprListRange u
extractChildRanges (UTerm u) = [ extractRangeTerm u]

extractChildExprRange :: Expr -> [LineRange]
extractChildExprRange (Special _ e _) = [ extractRangeExpr e ]
extractChildExprRange (Dispatch e _) = [ extractRangeExpr e ]
extractChildExprRange (Collection _ sel _) = [ extractRangeSepExprList sel ]
extractChildExprRange (Term t _) = [ extractRangeTerm t ]
extractChildExprRange (Comment s _) = []
extractChildExprRange (Seq e1 e2 _) = [extractRangeExpr e1, extractRangeExpr e2]
extractChildExprRange (Empty _) = []

extractChildSepExprListRange :: SepExprList -> [LineRange]
extractChildSepExprListRange (Nil _) = []
extractChildSepExprListRange (Cons e _ sel _) = [ extractRangeExpr e, extractRangeSepExprList sel ]

instance (Monad m) => OracleF GroupedDiffOracle m where
  callF o@(GroupedDiffOracle (diffActions, copyMaps)) s d = do
    let guard = deOptimize copyMaps s d
    let sRange = extractRange s
    let dRange = extractRange d
    if guard then do
      -- traceM "Guard is true"
      -- traceM ("src[" ++ show sRange ++ "]: " ++ show s)
      -- traceM ("dst[" ++ show dRange ++ "]: " ++ show d)
      return [S]
    else do
      let ans = askOracle o s d
      return ans



instance (Monad m) => OracleP GroupedDiffOracle m where
  callP _ An         An         = do
    -- traceM "empty"
    return []
  callP _ An         (_ `Ac` _) = do
    -- traceM "I"
    return [ I ]
  callP _ (_ `Ac` _) An         = do
    -- traceM "D"
    return [ D ]
  callP o@(GroupedDiffOracle (diffActions, copyMaps)) (s `Ac` _) (d `Ac` _) =  do
    let guard = deOptimize copyMaps s d
    let sRange = extractRange s
    let dRange = extractRange d
    if guard then do
      -- traceM "Guard is true"
      -- traceM ("src[" ++ show sRange ++ "]: " ++ show s)
      -- traceM ("dst[" ++ show dRange ++ "]: " ++ show d)
      return [S]
    else do
      let ans = askOracle o s d
      return ans

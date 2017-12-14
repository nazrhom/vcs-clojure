{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Oracle.DiffOracle where

import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace

import Control.Monad.Reader
import qualified Data.IntMap as M
import qualified Data.Set as S

import Oracle.Internal
import Language.Common
import Language.Clojure.AST
import Language.Clojure.Lang
import Util.UnixDiff

type CopyMap = M.IntMap Int
type CopyMaps = (CopyMap, CopyMap)
type MbMoveConflict = ConflictResult (S.Set (Int, Int))
type DelInsMap = (M.IntMap Path, M.IntMap Path)
data DiffOracle = DiffOracle DelInsMap
  deriving (Show)

buildDiffOracle :: String -> String -> Expr -> Expr -> DiffOracle
buildDiffOracle s d src dst = DiffOracle diffActions
  where
    cp = buildCopyMaps (preprocess s d)
    delinsMap = buildDelInsMap (preprocessGrouped s d)
    diffActions = solveConflicts delinsMap cp src dst

buildCopyMaps :: [DiffAction] -> (M.IntMap Int, M.IntMap Int)
buildCopyMaps as = (insMap, reverseMap insMap)
  where
    insMap = buildCopyMap as
    reverseMap = M.fromList . map swap . M.toList

    buildCopyMap [] = (M.empty)
    buildCopyMap (first:rest) = (process first) `M.union` (buildCopyMap rest)
      where
        process (Copy (i1, i2)) = M.singleton i1 i2
        process _ = M.empty

solveConflicts :: DelInsMap -> CopyMaps -> Expr -> Expr -> DelInsMap
solveConflicts diffActions copyMaps src dst = foldl invalidate diffActions conflicts
  where
    conflicts = checkCopyMaps copyMaps src dst

invalidate :: DelInsMap -> [MbMoveConflict] -> DelInsMap
invalidate diffActions []                 = diffActions
invalidate diffActions (NoConflict:rest)  = invalidate diffActions rest
invalidate diffActions ((ConflictAt pairs):rest) = invalidate (S.foldl invalidatePair diffActions pairs) rest

invalidatePair :: DelInsMap -> (Int, Int) -> DelInsMap
invalidatePair (srcMap, dstMap) (i,j) = (M.insert i M srcMap, M.insert j M dstMap)



unionDelInsMap :: DelInsMap -> DelInsMap -> DelInsMap
unionDelInsMap (s1, d1) (s2, d2) = (M.union s1 s2, M.union d1 d2)

buildDelInsMap :: [GroupDiffAction] -> DelInsMap
buildDelInsMap []           = (M.empty, M.empty)
buildDelInsMap (first:rest) = process first `unionDelInsMap` buildDelInsMap rest
  where
    process (OMod srcRange dstRange) = (insertRange srcRange M, insertRange dstRange M)
    process (OIns dstRange _)        = (M.empty, insertRange dstRange I)
    process (ODel srcRange _)        = (insertRange srcRange D, M.empty)

insertRange :: LineRange -> Path -> M.IntMap Path
insertRange (Range s e) o = go s M.empty
  where
    go i m | i <= e    = go (i+1) (M.insert i o m)
           | otherwise = m

giveAdvice' :: DelInsMap -> Usingl u -> Usingl v -> [Path]
giveAdvice' (srcMap, dstMap) src dst =
  if (isMod srcRange srcMap && isMod dstRange dstMap)
    then []
  else if (isDel srcRange srcMap || isMod srcRange srcMap)
    then [ D ]
  else if (isIns dstRange dstMap || isMod dstRange dstMap)
    then [ I ]
  else [ M ]
  where
    srcRange = fromJust $ extractRange src
    dstRange = fromJust $ extractRange dst

isContainedIn :: LineRange -> LineRange -> Bool
isContainedIn (Range s1 e1) (Range s2 e2) = s2 <= s1 && e1 <= e2

isMod :: LineRange -> M.IntMap Path -> Bool
isMod lr m = case M.lookup (takeStart lr) m of
  Just M -> True
  _      -> False

isIns :: LineRange -> M.IntMap Path -> Bool
isIns lr m = case M.lookup (takeStart lr) m of
  Just I -> True
  _      -> False

isDel :: LineRange -> M.IntMap Path -> Bool
isDel lr m = case M.lookup (takeStart lr) m of
  Just D -> True
  _      -> False

inSync :: Usingl u -> LineRange -> Bool
inSync u lr = uRange `inSync'` lr
  where
    uRange = fromJust $ extractRange u
    inSync' :: LineRange -> LineRange -> Bool
    inSync' (Range s1 e1) (Range s2 e2) = s2 <= s1 && s1 <= e2

copySetExpr :: CopyMap -> Expr -> S.Set Int
copySetExpr copyMap e = collectAll copyMap eRange
  where
    eRange = extractRangeExpr e

copySetSel :: CopyMap -> SepExprList -> S.Set Int
copySetSel copyMap e = collectAll copyMap eRange
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

lookupSet :: S.Set Int -> CopyMap -> S.Set Int
lookupSet s cp = S.map (fromJust . flip M.lookup cp) s

lookupSetPairs :: S.Set Int -> CopyMap -> [(Int, Int)]
lookupSetPairs s cp = S.toList $ S.map (\i -> (i, fromJust (M.lookup i cp))) s

intersectsNonOverlapping :: Ord a => S.Set a -> S.Set a -> S.Set a -> S.Set a
intersectsNonOverlapping target a b =
    if check
    then target `S.difference` overlapping
    else S.empty

  where
    check = target `intersects` (a `S.difference` overlapping) && target `intersects` (b `S.difference` overlapping)
    overlapping  = a `S.intersection` b
    targetL = S.toList target

intersects :: Ord a => S.Set a -> S.Set a -> Bool
intersects a b = not (S.null (a `S.intersection` b))

pickBigger :: CopyMaps -> S.Set Int -> S.Set Int -> S.Set (Int, Int)
pickBigger (srcMap, dstMap) a b
  | S.size a >= S.size b = S.fromList $ map swap (lookupSetPairs a dstMap)
  | otherwise            = S.fromList $ lookupSetPairs b srcMap


deOptimizeExpr :: CopyMaps -> Expr -> Expr -> MbMoveConflict
deOptimizeExpr cp@(srcMap, dstMap) (Seq a b _) (Seq c d _) =
    if (S.null overlapA && S.null overlapC)
    then NoConflict
    else ConflictAt (pickBigger cp overlapA overlapC)
  where
    copySetA = copySetExpr srcMap a
    copySetB = copySetExpr srcMap b
    copySetC = copySetExpr dstMap c
    copySetD = copySetExpr dstMap d
    copyTargetA = lookupSet copySetA srcMap
    copyTargetC = lookupSet copySetC dstMap
    overlapA = intersectsNonOverlapping copyTargetA copySetC copySetD
    overlapC = intersectsNonOverlapping copyTargetC copySetA copySetB
deOptimizeExpr cp@(srcMap, dstMap) (Collection _ (Cons a _ b _) _) (Collection _ (Cons c _ d _) _) =
    if (S.null overlapA && S.null overlapC)
    then NoConflict
    else ConflictAt (pickBigger cp overlapA overlapC)
  where
    copySetA = copySetExpr srcMap a
    copySetB = copySetSel srcMap b
    copySetC = copySetExpr dstMap c
    copySetD = copySetSel dstMap d
    copyTargetA = lookupSet copySetA srcMap
    copyTargetC = lookupSet copySetC dstMap
    overlapA = intersectsNonOverlapping copyTargetA copySetC copySetD
    overlapC = intersectsNonOverlapping copyTargetC copySetA copySetB
deOptimizeExpr _ _ _  = NoConflict

checkCopyMaps :: CopyMaps -> Expr -> Expr -> [[MbMoveConflict]]
checkCopyMaps cp@(srcMap, dstMap) src dst = fmap (collectSrcDstLines cp src dst) (M.toList srcMap)

collectSrcDstLines :: CopyMaps -> Expr -> Expr -> (Int, Int) -> [MbMoveConflict]
collectSrcDstLines cp src dst (s,d) = map (\(src,dst) -> deOptimizeExpr cp (wrap src) (wrap dst)) conflifts
  where
    conflifts = zipEqLen srcConflicts dstConflicts
    srcConflicts = collectSubTrees src s
    dstConflicts = collectSubTrees dst d

zipEqLen :: [a] -> [b] -> [(a,b)]
zipEqLen (a:as) (b:bs) = (a,b):(zipEqLen as bs)
zipEqLen []         [] = []
zipEqLen []         bs = error "dst is longer"
zipEqLen as         [] = error "src is longer"

wrap :: SubTree -> Expr
wrap (Exp e) = e
wrap (Sel sel) = (Collection Parens sel (extractRangeSepExprList sel))

instance (Monad m) => OracleF DiffOracle m where
  callF o@(DiffOracle diffActions) s d = return $ askOracle o s d

instance (Monad m) => OracleP DiffOracle m where
  callP _ An         An         = do
    return []
  callP _ An         (_ `Ac` _) = do
    return [ I ]
  callP _ (_ `Ac` _) An         = do
    return [ D ]
  callP o@(DiffOracle diffActions) (s `Ac` _) (d `Ac` _) = return $ askOracle o s d

askOracle :: DiffOracle -> Usingl u -> Usingl v -> [Path]
askOracle (DiffOracle diffActions) src dst = case (extractRange src, extractRange dst) of
      (Nothing, Nothing)         -> [ M ]
      (Just sRange, Nothing)     -> [ D ]
      (Nothing, Just dRange)     -> [ I ]
      (Just sRange, Just dRange) -> giveAdvice' diffActions src dst
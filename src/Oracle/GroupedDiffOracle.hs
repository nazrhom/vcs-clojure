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

import Oracle.Internal
import Clojure.Lang
import Clojure.AST
import Util.UnixDiff

data GroupedDiffOracle = GroupedDiffOracle [GroupDiffAction]

askOracle :: GroupedDiffOracle -> Usingl u -> Usingl v -> [Path]
askOracle (GroupedDiffOracle o) src dst = case (extractRange src, extractRange dst) of
      (Nothing, Nothing)         -> [ M ]
      (Just sRange, Nothing)     -> [ D ]
      (Nothing, Just dRange)     -> [ I ]
      (Just sRange, Just dRange) -> giveAdvice o src dst
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
  else if src `shouldMod` srcLr && not (dst `shouldMod` dstLr) then
    [ D ]
  else if dst `shouldMod` dstLr && not (src `shouldMod` srcLr) then
    [ I ]
  else giveAdvice os src dst
  where
    dstRange = fromJust $ extractRange dst
    srcRange = fromJust $ extractRange src

giveAdvice ((OIns lr i):os) src dst =
  if dstRange `inSync` lr
  then [ I ]
  else giveAdvice os src dst
  where
    dstRange = fromJust $ extractRange dst

giveAdvice ((ODel lr i):os) src dst =
  if srcRange `inSync` lr
  then [ D ]
  else giveAdvice os src dst
  where
    srcRange = fromJust $ extractRange src

inSync :: LineRange -> LineRange -> Bool
inSync (Range s1 _) (Range s2 _) = s1 == s2

isContainedIn :: LineRange -> LineRange -> Bool
isContainedIn (Range s1 e1) (Range s2 e2) = s2 <= s1 && s1 <= e2-- && e1 <= e2

shouldMod :: Usingl u -> LineRange -> Bool
shouldMod u lr = uRange `isContainedIn` lr
-- || any (flip isContainedIn lr) children
  where
    uRange = fromJust $ extractRange u
    children = extractChildRanges u

extractChildRanges :: Usingl u -> [LineRange]
extractChildRanges (UString u) = []
extractChildRanges (UExpr u)   = extractChildExprRange u
extractChildRanges (USepExprList u) = extractChildSepExprListRange u
extractChildRanges (UTerm u) = []

extractChildExprRange :: Expr -> [LineRange]
extractChildExprRange (Special _ e _) = [ extractRangeExpr e ]
extractChildExprRange (Dispatch e _) = [ extractRangeExpr e ]
extractChildExprRange (Collection _ sel _) = [ extractRangeSepExprList sel ]
extractChildExprRange (Term t _) = [ extractRangeTerm t ]
extractChildExprRange (Comment s _) = []
extractChildExprRange (Seq e1 e2 _) = [extractRangeExpr e1, extractRangeExpr e2]

extractChildSepExprListRange :: SepExprList -> [LineRange]
extractChildSepExprListRange (Nil _) = []
extractChildSepExprListRange (Singleton e _) = [ extractRangeExpr e ]
extractChildSepExprListRange (Cons e _ sel _) = [ extractRangeExpr e, extractRangeSepExprList sel ]

-- s2 s1 e2 e1
instance (Monad m) => OracleF GroupedDiffOracle m where
  callF o s d = do
    let sRange = extractRange s
    let dRange = extractRange d
    -- traceM ("src[" ++ show sRange ++ "]: " ++ show s)
    -- traceM ("dst[" ++ show dRange ++ "]: " ++ show d)
    let ans = askOracle o s d
    -- traceM ("ans: " ++ show ans)
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
  callP o (s `Ac` _) (d `Ac` _) = return $ askOracle o s d
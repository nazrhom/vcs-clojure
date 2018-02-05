{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module VCS.Diff where

import Control.Applicative
import Data.Type.Equality hiding (apply)
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

import VCS.Apply
import VCS.Multirec
import VCS.Cost
import Language.Clojure.Lang
import Language.Clojure.Cost
import Language.Common

import Oracle.Oracle

type DiffAlMu o m rec
  = forall r . (IsRecEl r , MonadOracle o m)
            => o -> Int -> Usingl r -> Usingl r -> HistoryM m (rec r)

diffAt :: (MonadOracle o m) => forall rec . DiffAlMu o m rec
       -> o -> Int -> Usingl a -> Usingl a -> HistoryM m (At rec a)
diffAt diffR orc maxCost x y = do
  cost <- getCurrentCost
  let costP = costPair x y
  guard (cost+costP <= maxCost)
  updateCost costP
  onRecursiveGuy
    (\y -> Ai <$> (diffR orc maxCost x y))
    (\ny -> return $ As (Contract (x, ny)))
    y

diffS :: (IsRecEl a, MonadOracle o m) => forall rec . DiffAlMu o m rec
      -> (forall a . rec a -> Int)
      -> o -> Int -> Usingl a -> Usingl a -> HistoryM m (Spine (At rec) (Al (At rec)) a)
diffS diffR costR orc maxCost s1 s2 =
  mapSpineM (uncurry (diffAt diffR orc maxCost) . unContract)
            (uncurryPair $ alignP diffR costR orc maxCost)
            (spine s1 s2)
  where
    alignP :: (MonadOracle o m)
           => DiffAlMu o m rec
           -> (forall a . rec a -> Int)
           -> o -> Int -> All Usingl s -> All Usingl d -> HistoryM m (Al (At rec) s d)
    alignP diffR costR orc maxCost p1 p2 = do
      cost <- getCurrentCost
      (al, cost') <- liftH $ align orc maxCost cost p1 p2
      updateCost (cost' - cost)
      (mapAlM (uncurry (diffAt diffR orc maxCost) . unContract) al)

costPair :: Usingl u -> Usingl u -> Int
costPair x y = onRecursiveGuy (const 0) (\nr -> costK (Contract (x,nr))) y

diffInsCtx :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> HistoryM m (Ctx (AtmuPos v) p)
diffInsCtx orc maxCost x An = empty
diffInsCtx orc maxCost x (y `Ac` ay)
  = onRecursiveGuy (rec orc maxCost x ay) (nonrec orc maxCost x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl v -> All Usingl p -> Usingl u
        -> HistoryM m (Ctx (AtmuPos v) (u ': p))
    rec orc maxCost x ys y = do
      cost <- getCurrentCost
      let rest = costAll ys
      if (cost + rest) <= maxCost
      then (do
        updateCost rest
        (flip Here ys <$> FixPos <$> (diffAlmuO orc maxCost x y))) <|> (nonrec orc maxCost x ys y)
      else (nonrec orc maxCost x ys y)

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (AtmuPos v) (u ': p))
    nonrec orc maxCost x ys y = do
      cost <- getCurrentCost
      let next = costUsingl y
      guard (cost+next <= maxCost)
      updateCost next
      There y <$> (diffInsCtx orc maxCost x ys)

diffDelCtx :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> All Usingl p -> Usingl v -> HistoryM m (Ctx (AtmuNeg v) p)
diffDelCtx orc maxCost An x = empty
diffDelCtx orc maxCost (y `Ac` ay) x
  = onRecursiveGuy (rec orc maxCost x ay) (nonrec orc maxCost x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl v -> All Usingl p -> Usingl u
        -> HistoryM m (Ctx (AtmuNeg v) (u ': p))
    rec orc maxCost x ys y = do
      cost <- getCurrentCost
      let rest = costAll ys
      if (cost + rest) <= maxCost
      then (do
        updateCost rest
        (flip Here ys <$> FixNeg <$> (diffAlmuO orc maxCost y x))) <|> (nonrec orc maxCost x ys y)
      else (nonrec orc maxCost x ys y)

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (AtmuNeg v) (u ': p))
    nonrec orc maxCost x ys y = do
      cost <- getCurrentCost
      let next = costUsingl y
      guard (cost+next <= maxCost)
      updateCost next
      There y <$> (diffDelCtx orc maxCost ys x)

diffAlmu :: (IsRecEl u, IsRecEl v, MonadOracle o m)
         => o -> Int -> Usingl u -> Usingl v -> m (Almu u v)
diffAlmu orc maxCost x y =
  evalHistory initialHistory 0 (diffAlmuO orc maxCost x y)

diffAlmuO :: (IsRecEl u, IsRecEl v, MonadOracle o m)
          => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffAlmuO o maxCost x y = callF o x y >>= pursue o maxCost x y
  where
    pursue :: (IsRecEl u , IsRecEl v , MonadOracle o m)
           => o -> Int -> Usingl u -> Usingl v -> [Path] -> HistoryM m (Almu u v)
    pursue o maxCost x y [] = empty
    pursue o maxCost x y (i:is) = follow o maxCost x y i <|> pursue o maxCost x y is

    follow :: (IsRecEl u , IsRecEl v , MonadOracle o m)
           => o -> Int -> Usingl u -> Usingl v -> Path -> HistoryM m (Almu u v)
    follow o maxCost x y I = diffIns o maxCost x y
    follow o maxCost x y D = diffDel o maxCost x y
    follow o maxCost x y M = diffMod o maxCost x y

diffMod :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffMod orc maxCost s1 s2 = case (testEquality s1 s2, sameDepth s1 s2) of
  (Just Refl, True) -> Alspn <$> (diffS diffAlmuH costAlmuH orc maxCost s1 s2)
  (_, _) -> empty
  where
    diffAlmuH :: (IsRecEl u, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl u -> HistoryM m (AlmuH u)
    diffAlmuH o maxCost x y = AlmuH <$> local (liftPath (M:)) (diffAlmuO o maxCost x y)

diffIns :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffIns orc maxCost x s = case view s of
  (Tag c p) -> do
    cost <- getCurrentCost
    guard (cost+1 <= maxCost)
    updateCost 1
    Alins c <$> local (I:) (diffInsCtx orc maxCost x p)

diffDel :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffDel orc maxCost s x = case (view s) of
  (Tag c p) -> do
    cost <- getCurrentCost
    guard (cost+1 <= maxCost)
    updateCost 1
    Aldel c <$> local (D:) (diffDelCtx orc maxCost p x)

spine :: IsRecEl r => Usingl r -> Usingl r -> Spine TrivialA TrivialP r
spine x y | x == y = Scp
spine x y | otherwise = case (view x, view y) of
  ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
    Just Refl -> Scns c1 (zipP l1 l2)
    Nothing -> Schg c1 c2 (Pair l1 l2)

-- |Implement DP-style optimization for Alignments and Recursive alignmetns.
--
-- The phase records the LAST decision took by the algo (either an insertion,
-- modification ordeletion)
align :: (MonadOracle o m)
      => o -> Int -> Int -> All Usingl p1 -> All Usingl p2
      -> m (Al TrivialA p1 p2, Int)
align orc maxCost currCost p1 p2 = runHistory initialHistory currCost (alignO orc maxCost p1 p2)

alignO :: (MonadOracle o m)
       => o -> Int -> All Usingl p1 -> All Usingl p2
       -> HistoryM m (Al TrivialA p1 p2)
alignO orc maxCost An An = return A0
alignO orc maxCost p1 p2 = do
  paths <- callP orc p1 p2
  followAllPaths paths orc maxCost p1 p2

followAllPaths :: (MonadOracle o m)
               => [Path] -> o -> Int -> All Usingl p1 -> All Usingl p2
               -> HistoryM m (Al TrivialA p1 p2)
followAllPaths []     _   _   _  _
  = empty
followAllPaths (i:is) orc maxCost p1 p2
  = (followPath i orc maxCost p1 p2) <|> (followAllPaths is orc maxCost p1 p2)


-- * Follows one specific path. Makes sure the recursive call to
--   alignO has access to this path, for later inspection.
followPath :: (MonadOracle o m)
           => Path -> o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
followPath _ orc maxCost An An         = pure A0
followPath I orc maxCost x y = alignIns orc maxCost x y
followPath D orc maxCost x y = alignDel orc maxCost x y
followPath M orc maxCost x y = alignMod orc maxCost x y


liftPath :: ([Path] -> [Path]) -> History -> History
liftPath f = liftHistory (f,id)

-- liftCost :: (Int -> Int) -> History -> History
-- liftCost f = liftHistory (id,f)

liftHistory :: (([Path] -> [Path], Int -> Int)) -> History -> History
liftHistory (fp, fc) h = fp h

alignIns :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignIns orc maxCost p1 (a `Ac` p) = do
  cost <- getCurrentCost
  let costA = costUsingl a
  guard (cost+costA <= maxCost)
  updateCost costA
  Ains a <$> local (I:) (alignO orc maxCost p1 p)

alignDel :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignDel orc maxCost (a `Ac` p) p2 = do
  cost <- getCurrentCost
  let costA = costUsingl a
  guard (cost+costA <= maxCost)
  updateCost costA
  Adel a <$> local (D:) (alignO orc maxCost p p2)

alignMod :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignMod orc maxCost (a1 `Ac` p1) (a2 `Ac` p2) =
  case testEquality a1 a2 of
    Just Refl -> do
      cost <- getCurrentCost
      let costP = costPair a1 a2
      guard (cost+costP <= maxCost)
      updateCost costP
      (Amod (Contract (a1, a2)) <$> local (M:) (alignO orc maxCost p1 p2))
    Nothing -> empty


--- Utility
uncurryPair :: (All Usingl s -> All Usingl d -> res)
            -> TrivialP s d -> res
uncurryPair f (Pair l r) = f l r



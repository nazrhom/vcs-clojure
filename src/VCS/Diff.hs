{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
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
diffAt diffR orc maxCost x =
  onRecursiveGuy
    (\y -> Ai <$> (diffR orc maxCost x y))
    (\ny -> return $ As (Contract (x, ny)))

diffS :: (IsRecEl a, MonadOracle o m) => forall rec . DiffAlMu o m rec
      -> (forall a . rec a -> Int)
      -> o -> Int -> Usingl a -> Usingl a -> HistoryM m (Spine (At rec) (Al (At rec)) a)
diffS diffR costR orc maxCost s1 s2 =
  mapSpineH (uncurry (diffAt diffR orc maxCost) . unContract)
            (uncurryPair $ alignP diffR costR orc maxCost)
            (costAt costR)
            (maxCost)
            (spine s1 s2)
  where
    alignP :: (MonadOracle o m)
           => DiffAlMu o m rec
           -> (forall a . rec a -> Int)
           -> o -> Int -> All Usingl s -> All Usingl d -> HistoryM m (Al (At rec) s d)
    alignP diffR costR orc maxCost p1 p2 = do
      cost <- getCurrentCost
      al <- lift $ align orc maxCost cost p1 p2
      (mapAlH (uncurry (diffAt diffR orc maxCost) . unContract)
              (costAt costR)
              (maxCost)
              al)

mapAlH :: (Alternative m, Monad m) =>
           (forall a . at1 a -> HistoryM m (at2 a))
        -> (forall a . at2 a -> Int)
        -> Int
        -> Al at1 s d -> HistoryM m (Al at2 s d)
mapAlH f costAt maxCost A0           = return A0
mapAlH f costAt maxCost (Adel at al) = do
  cost <- getCurrentCost
  let costA = costUsingl at
  if cost + costA <= maxCost
  then local (liftCost (+costA)) (Adel at <$> mapAlH f costAt maxCost al)
  else empty
mapAlH f costAt maxCost (Ains at al) = do
  cost <- getCurrentCost
  let costA = costUsingl at
  if cost + costA <= maxCost
  then local (liftCost (+costA)) (Ains at <$> mapAlH f costAt maxCost al)
  else empty
mapAlH f costAt maxCost (Amod at al) = do
  cost <- getCurrentCost
  at' <- f at
  let costat = costAt at'
  if cost + costat <= maxCost
  then local (liftCost (+costat)) (Amod <$> pure at' <*> mapAlH f costAt maxCost al)
  else empty

mapSpineH :: (Alternative m, Monad m) => (forall a . at1 a -> HistoryM m (at2 a))
     -> (forall s d . al1 s d -> HistoryM m (al2 s d))
     -> (forall a . at2 a -> Int)
     -> Int
     -> Spine at1 al1 u -> HistoryM m (Spine at2 al2 u)
mapSpineH f g  _      _       Scp
  = return Scp
mapSpineH f g costAt maxCost (Scns c ps)
  = Scns c <$> mapAllH f costAt maxCost ps
mapSpineH f g costAt _       (Schg c1 c2 al)
  = Schg c1 c2 <$> g al

mapAllH :: (Alternative m , Monad m) => (forall a . p a -> HistoryM m (q a))
        -> (forall a . q a -> Int)
        -> Int -> All p xs -> HistoryM m (All q xs)
mapAllH f _  _ An = return An
mapAllH f costAt maxCost (px `Ac` pxs) = do
  cost <- getCurrentCost
  k <- f px
  let costPx = costAt k
  if cost + costPx <= maxCost
  then local (liftCost (+costPx)) (Ac <$> pure k <*> mapAllH f costAt maxCost pxs)
  else empty

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
      then (flip Here ys <$> FixPos <$> local (liftCost (+rest)) (diffAlmuO orc maxCost x y)) <|> (nonrec orc maxCost x ys y)
      else (nonrec orc maxCost x ys y)

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (AtmuPos v) (u ': p))
    nonrec orc maxCost x ys y = do
      cost <- getCurrentCost
      let next = costUsingl y
      if (cost + next) <= maxCost
      then (There y <$> local (liftCost (+next)) (diffInsCtx orc maxCost x ys))
      else empty

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
      then (flip Here ys <$> FixNeg <$> local (liftCost (+rest)) (diffAlmuO orc maxCost y x)) <|> (nonrec orc maxCost x ys y)
      else (nonrec orc maxCost x ys y)

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (AtmuNeg v) (u ': p))
    nonrec orc maxCost x ys y = do
      cost <- getCurrentCost
      let next = costUsingl y
      if (cost + next) <= maxCost
      then (There y <$> local (liftCost (+next)) (diffDelCtx orc maxCost ys x))
      else empty

diffAlmu :: (IsRecEl u, IsRecEl v, MonadOracle o m)
         => o -> Int -> Usingl u -> Usingl v -> m (Almu u v)
diffAlmu orc maxCost x y = runReaderT (diffAlmuO orc maxCost x y) initialHistory

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
    -- follow o x y FM = diffModUnsafe o x y
    -- follow o x y S = diffAll NoDupBranches x y

-- diffAll :: (IsRecEl u, IsRecEl v, MonadOracle o m)
--           => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
-- diffAll o x y = diffMod o x y <|> diffIns o x y <|> diffDel o x y

-- diffModUnsafe :: (IsRecEl u, IsRecEl v, MonadOracle o m)
--         => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
-- diffModUnsafe orc s1 s2 = case testEquality s1 s2 of
--   Just Refl -> Alspn <$> diffS (toH diffAlmuO) orc s1 s2
--   Nothing -> case unsafeCoerceEquality s1 s2 of
--       Just Refl -> do
--         -- traceM "UnsafeCoerce"
--         Alspn <$> diffS (toH diffAlmuO) orc s1 s2
--     -- diffIns orc s1 s2 <|> diffDel orc s1 s2



diffMod :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffMod orc maxCost s1 s2 = case testEquality s1 s2 of
  Just Refl -> Alspn <$> (diffS diffAlmuH costAlmuH orc maxCost s1 s2)
  Nothing -> empty
  where
    diffAlmuH :: (IsRecEl u, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl u -> HistoryM m (AlmuH u)
    diffAlmuH o maxCost x y = AlmuH <$> local (liftPath (M:)) (diffAlmuO o maxCost x y)

diffIns :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffIns orc maxCost x s = case view s of
  (Tag c p) -> do
    cost <- getCurrentCost
    if cost + 1 <= maxCost
    then Alins c <$> local (liftHistory ((I:), (+1))) (diffInsCtx orc maxCost x p)
    else empty

diffDel :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffDel orc maxCost s x = case (view s) of
  (Tag c p) -> do
    cost <- getCurrentCost
    if cost + 1 <= maxCost
    then Aldel c <$> local (liftHistory ((D:), (+1))) (diffDelCtx orc maxCost p x)
    else empty


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
      -> m (Al TrivialA p1 p2)
align orc maxCost currCost p1 p2 = runReaderT (alignO orc maxCost p1 p2) (History { path = [I,M,D], cost = currCost })

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

liftCost :: (Int -> Int) -> History -> History
liftCost f = liftHistory (id,f)

liftHistory :: (([Path] -> [Path], Int -> Int)) -> History -> History
liftHistory (fp, fc) h
  = History {
      path = fp (path h)
    , cost = fc (cost h)
    }

alignIns :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignIns orc maxCost p1 (a `Ac` p) =
  Ains a <$> local (liftPath (I:)) (alignO orc maxCost p1 p)

alignDel :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignDel orc maxCost (a `Ac` p) p2 =
  Adel a <$> local (liftPath (D:)) (alignO orc maxCost p p2)

alignMod :: (MonadOracle o m)
           => o -> Int -> All Usingl p1 -> All Usingl p2
           -> HistoryM m (Al TrivialA p1 p2)
alignMod orc maxCost (a1 `Ac` p1) (a2 `Ac` p2) =
  case testEquality a1 a2 of
    Just Refl -> Amod (Contract (a1, a2)) <$> local (liftPath (M:)) (alignO orc maxCost p1 p2)
    Nothing -> empty



--- Utility
uncurryPair :: (All Usingl s -> All Usingl d -> res)
            -> TrivialP s d -> res
uncurryPair f (Pair l r) = f l r



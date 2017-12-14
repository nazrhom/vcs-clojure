{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module VCS.DiffPartial where

import Control.Applicative
import Data.Type.Equality hiding (apply)
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

import VCS.Apply
import VCS.Multirec
import VCS.Cost
import VCS.Diff
import Language.Clojure.Lang
import Language.Clojure.Cost
import Language.Common

import Oracle.Oracle

parallelDiffAlmu :: (IsRecEl u, IsRecEl v, IsRecEl w, MonadOracle o m)
             => o -> Int -> Usingl u -> Usingl v -> Usingl w
             -> m (PartialAlmu u v, PartialAlmu u w)
parallelDiffAlmu orc maxCost x y z = do
  xy <- diffPartialAlmu orc maxCost x y
  xz <- diffPartialAlmu orc maxCost x z
  return (xy, xz)

stepAlmu :: (IsRecEl u, IsRecEl v, Monad m, Alternative m)
         => m (PartialAlmu u v)
         -> m (PartialAlmu u v)
stepAlmu almus = do
  almu <- almus
  case almu of
    TSpn s -> undefined

diffPartialAlmu :: (IsRecEl u, IsRecEl v, MonadOracle o m)
         => o -> Int -> Usingl u -> Usingl v -> m (PartialAlmu u v)
diffPartialAlmu orc maxCost x y = runReaderT (diffPartialAlmuO orc maxCost x y) initialHistory


diffPartialAlmuO :: (IsRecEl u, IsRecEl v, MonadOracle o m)
          => o -> Int -> Usingl u -> Usingl v -> HistoryM m (PartialAlmu u v)
diffPartialAlmuO o maxCost x y = callF o x y >>= pursue o maxCost x y
  where
    pursue :: (IsRecEl u , IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl u -> Usingl v -> [Path] -> HistoryM m (PartialAlmu u v)
    pursue o maxCost x y [] = empty
    pursue o maxCost x y (i:is) = follow o maxCost x y i <|> pursue o maxCost x y is

    follow :: (IsRecEl u , IsRecEl v , MonadOracle o m)
           => o -> Int -> Usingl u -> Usingl v -> Path -> HistoryM m (PartialAlmu u v)
    follow o maxCost x y I = diffPartialIns o maxCost x y
    follow o maxCost x y D = diffPartialDel o maxCost x y
    follow o maxCost x y M = diffPartialMod o maxCost x y

diffPartialMod :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (PartialAlmu u v)
diffPartialMod orc maxCost s1 s2 = case testEquality s1 s2 of
  Just Refl -> TSpn <$> (diffPartialS pairUp undefined orc maxCost s1 s2)
  Nothing -> empty
  where
    pairUp :: (IsRecEl r, MonadOracle o m)
       => o -> Int -> Usingl r -> Usingl r -> HistoryM m (AlmuP r)
    pairUp _ _ x y = AlmuBase <$> Contract <$> pure (x,y)

diffPartialIns :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (PartialAlmu u v)
diffPartialIns orc maxCost x s = case view s of
  (Tag c p) -> do
    cost <- getCurrentCost
    if cost + 1 <= maxCost
    then TIns c <$> local (liftHistory ((I:), (+1))) (gatherCtxIns orc maxCost x p)
    else empty

diffPartialDel :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl u -> Usingl v -> HistoryM m (PartialAlmu u v)
diffPartialDel orc maxCost s x = case (view s) of
  (Tag c p) -> do
    cost <- getCurrentCost
    if cost + 1 <= maxCost
    then TDel c <$> local (liftHistory ((D:), (+1))) (gatherCtxDel orc maxCost p x)
    else empty

gatherCtxIns :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> HistoryM m (Ctx (PartialPos v) p)
gatherCtxIns orc maxCost x An = empty
gatherCtxIns orc maxCost x (y `Ac` ay)
  = onRecursiveGuy (rec orc maxCost x ay) (nonrec orc maxCost x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl v -> All Usingl p -> Usingl u
        -> HistoryM m (Ctx (PartialPos v) (u ': p))
    rec orc maxCost x ys y = do
      cost <- getCurrentCost
      let rest = costAll ys
      if (cost + rest) <= maxCost
      then (flip Here ys <$> TPos <$> pure (x,y)) <|> (nonrec orc maxCost x ys y)
      else (nonrec orc maxCost x ys y)

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (PartialPos v) (u ': p))
    nonrec orc maxCost x ys y = do
      cost <- getCurrentCost
      let next = costUsingl y
      if (cost + next) <= maxCost
      then (There y <$> (gatherCtxIns orc maxCost x ys))
      else empty

gatherCtxDel :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> All Usingl p -> Usingl v -> HistoryM m (Ctx (PartialNeg v) p)
gatherCtxDel orc maxCost An x = empty
gatherCtxDel orc maxCost (y `Ac` ay) x
  = onRecursiveGuy (rec orc maxCost x ay) (nonrec orc maxCost x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, MonadOracle o m)
        => o -> Int -> Usingl v -> All Usingl p -> Usingl u
        -> HistoryM m (Ctx (PartialNeg v) (u ': p))
    rec orc maxCost x ys y = do
      cost <- getCurrentCost
      let rest = costAll ys
      if (cost + rest) <= maxCost
      then (flip Here ys <$> TNeg <$> pure (y, x)) <|> (nonrec orc maxCost x ys y)
      else (nonrec orc maxCost x ys y)

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Int -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (PartialNeg v) (u ': p))
    nonrec orc maxCost x ys y = do
      cost <- getCurrentCost
      let next = costUsingl y
      if (cost + next) <= maxCost
      then (There y <$> (gatherCtxDel orc maxCost ys x))
      else empty

diffPartialS :: (IsRecEl a, MonadOracle o m) => DiffAlMu o m rec
      -> (forall a . rec a -> Int)
      -> o -> Int -> Usingl a -> Usingl a -> HistoryM m (Spine (At rec) (Al (At rec)) a)
diffPartialS diffR costR orc maxCost s1 s2 =
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

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
import Clojure.Lang
import Oracle.Oracle

type DiffAlMu o m rec
  = forall r . (IsRecEl r , MonadOracle o m)
            => o -> Usingl r -> Usingl r -> HistoryM m (rec r)

diffAt :: (MonadOracle o m) => forall rec . DiffAlMu o m rec
       -> o -> Usingl a -> Usingl a -> HistoryM m (At rec a)
diffAt diffR orc x = onRecursiveGuy (\y -> Ai <$> (diffR orc x y))
                                    (\ny -> return (As (Contract (x , ny))))


diffS :: (IsRecEl a, MonadOracle o m) => forall rec . DiffAlMu o m rec
      -> o -> Usingl a -> Usingl a -> HistoryM m (Spine (At rec) (Al (At rec)) a)
diffS diffR orc s1 s2
  = mapSpineM (uncurry (diffAt diffR orc) . unContract)
              (uncurryPair $ alignP diffR orc)
              (spine s1 s2)
  where
    alignP :: (MonadOracle o m)
           => DiffAlMu o m rec
           -> o -> All Usingl s -> All Usingl d -> HistoryM m (Al (At rec) s d)
    alignP diffR orc p1 p2 = do
      al <- lift $ align orc p1 p2
      mapAlM (uncurry (diffAt diffR orc) . unContract) al

diffInsCtx :: (IsRecEl v, MonadOracle o m)
           => o -> Usingl v -> All Usingl p -> HistoryM m (Ctx (AtmuPos v) p)
diffInsCtx orc x An = empty
diffInsCtx orc x (y `Ac` ay)
  = onRecursiveGuy (rec orc x ay) (nonrec orc x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, MonadOracle o m)
        => o -> Usingl v -> All Usingl p -> Usingl u
        -> HistoryM m (Ctx (AtmuPos v) (u ': p))
    rec orc x ys y = (flip Here ys <$> FixPos <$> diffAlmuO orc x y)
                  <|> nonrec orc x ys y

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (AtmuPos v) (u ': p))
    nonrec orc x ys y = There y <$> diffInsCtx orc x ys

diffDelCtx :: (IsRecEl v, MonadOracle o m)
           => o -> All Usingl p -> Usingl v -> HistoryM m (Ctx (AtmuNeg v) p)
diffDelCtx orc An x = empty
diffDelCtx orc (y `Ac` ay) x
  = onRecursiveGuy (rec orc x ay) (nonrec orc x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, MonadOracle o m)
        => o -> Usingl v -> All Usingl p -> Usingl u
        -> HistoryM m (Ctx (AtmuNeg v) (u ': p))
    rec orc x ys y = (flip Here ys <$> FixNeg <$> diffAlmuO orc y x)
                 <|> nonrec orc x ys y

    nonrec :: (IsRecEl v, MonadOracle o m)
           => o -> Usingl v -> All Usingl p -> Usingl u
           -> HistoryM m (Ctx (AtmuNeg v) (u ': p))
    nonrec orc x ys y = There y <$> diffDelCtx orc ys x

diffAlmu :: (IsRecEl u, IsRecEl v, MonadOracle o m)
         => o -> Usingl u -> Usingl v -> m (Almu u v)
diffAlmu orc x y = runReaderT (diffAlmuO orc x y) [I,M,D]

diffAlmuO :: (IsRecEl u, IsRecEl v, MonadOracle o m)
          => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffAlmuO o x y = callF o x y >>= pursue o x y
  where
    pursue :: (IsRecEl u , IsRecEl v , MonadOracle o m)
           => o -> Usingl u -> Usingl v -> [Path] -> HistoryM m (Almu u v)
    pursue o x y [] = empty
    pursue o x y (i:is) = follow o x y i <|> pursue o x y is

    follow :: (IsRecEl u , IsRecEl v , MonadOracle o m)
           => o -> Usingl u -> Usingl v -> Path -> HistoryM m (Almu u v)
    follow o x y I = diffIns o x y
    follow o x y D = diffDel o x y
    follow o x y M = diffMod o x y
    follow o x y FM = diffModUnsafe o x y
    follow o x y S = empty

diffMod :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffMod orc s1 s2 = case testEquality s1 s2 of
  Just Refl -> Alspn <$> diffS (toH diffAlmuO) orc s1 s2
  Nothing -> empty

diffModUnsafe :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffModUnsafe orc s1 s2 = case testEquality s1 s2 of
  Just Refl -> Alspn <$> diffS (toH diffAlmuO) orc s1 s2
  Nothing -> case unsafeCoerceEquality s1 s2 of
      Just Refl -> do
        -- traceM "UnsafeCoerce"
        Alspn <$> diffS (toH diffAlmuO) orc s1 s2
    -- diffIns orc s1 s2 <|> diffDel orc s1 s2

toH :: (IsRecEl u, MonadOracle o m)
    => (o -> Usingl u -> Usingl u -> HistoryM m (Almu u u))
    -> o -> Usingl u -> Usingl u -> HistoryM m (AlmuH u)
toH f o x y = AlmuH <$> local (M:) (f o x y)

diffIns :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffIns orc x s = case view s of
  (Tag c p) -> Alins c <$> local (I:) (diffInsCtx orc x p)

diffDel :: (IsRecEl u, IsRecEl v, MonadOracle o m)
        => o -> Usingl u -> Usingl v -> HistoryM m (Almu u v)
diffDel orc s x = case (view s) of
  (Tag c p) -> Aldel c <$> local (D:) (diffDelCtx orc p x)

--- Utility
uncurryPair :: (All Usingl s -> All Usingl d -> res)
            -> TrivialP s d -> res
uncurryPair f (Pair l r) = f l r

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Diff where

import Apply
import Multirec
import Lang
import Control.Applicative
import Data.Type.Equality hiding (apply)

diffAt :: (Monad m) => forall rec .
          (forall r . IsRecEl r => Phase -> Usingl r -> Usingl r -> m (rec r))
       -> Phase -> Usingl a -> Usingl a -> m (At rec a)
diffAt diffR d x = onRecursiveGuy (\y -> Ai <$> (diffR d x y))
                              (\ny -> return (As (Contract (x , ny))))


diffS :: (IsRecEl a, Monad m, Alternative m) => forall rec .
         (forall r . IsRecEl r => Phase -> Usingl r -> Usingl r -> m (rec r))
      -> Phase -> Usingl a -> Usingl a -> m (Spine (At rec) (Al (At rec)) a)
diffS diffR d s1 s2
  = mapSpineM (uncurry (diffAt diffR d) . unContract)
              (uncurryPair $ alignP diffR d)
              (spine s1 s2)
  where
    alignP :: (Monad m, Alternative m) => (forall r . IsRecEl r => Phase -> Usingl r -> Usingl r -> m (rec r))
           -> Phase -> All Usingl s -> All Usingl d -> m (Al (At rec) s d)
    alignP diffR d p1 p2 = do
      al <- align p1 p2
      mapAlM (uncurry (diffAt diffR d) . unContract) al

diffInsCtx :: (IsRecEl v, Alternative m, Monad m)
           => Usingl v -> All Usingl p -> m (Ctx (AtmuPos v) p)
diffInsCtx x An = empty
diffInsCtx x (y `Ac` ay)
  = onRecursiveGuy (rec x ay) (nonrec x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, Alternative m, Monad m)
        => Usingl v -> All Usingl p -> Usingl u
        -> m (Ctx (AtmuPos v) (u ': p))
    rec x ys y = (flip Here ys <$> FixPos <$> diffAlmu I x y)
              <|> nonrec x ys y

    nonrec :: (IsRecEl v, Alternative m, Monad m)
           => Usingl v -> All Usingl p -> Usingl u
           -> m (Ctx (AtmuPos v) (u ': p))
    nonrec x ys y = There y <$> diffInsCtx x ys

diffDelCtx :: (IsRecEl v, Alternative m, Monad m)
           => All Usingl p -> Usingl v -> m (Ctx (AtmuNeg v) p)
diffDelCtx An x = empty
diffDelCtx (y `Ac` ay) x
  = onRecursiveGuy (rec x ay) (nonrec x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v, Alternative m, Monad m)
        => Usingl v -> All Usingl p -> Usingl u
        -> m (Ctx (AtmuNeg v) (u ': p))
    rec x ys y = (flip Here ys <$> FixNeg <$> diffAlmu D y x)
              <|> nonrec x ys y

    nonrec :: (IsRecEl v, Alternative m, Monad m)
           => Usingl v -> All Usingl p -> Usingl u
           -> m (Ctx (AtmuNeg v) (u ': p))
    nonrec x ys y = There y <$> diffDelCtx ys x

diffAlmu :: (IsRecEl u, IsRecEl v, Alternative m, Monad m) => Phase -> Usingl u -> Usingl v -> m (Almu u v)
diffAlmu M x y = diffMod x y <|> diffIns x y <|> diffDel x y
diffAlmu I x y = diffMod x y <|> diffIns x y
diffAlmu D x y = diffMod x y <|> diffDel x y

diffMod :: (IsRecEl u, IsRecEl v, Alternative m, Monad m)
        => Usingl u -> Usingl v -> m (Almu u v)
diffMod s1 s2 = case testEquality s1 s2 of
  Just Refl -> Alspn <$> diffS (\p -> toH (diffAlmu p)) M s1 s2
  Nothing -> empty
  where
    toH :: (IsRecEl u, Alternative m)
        => (Usingl u -> Usingl u -> m (Almu u u))
        -> Usingl u -> Usingl u -> m (AlmuH u)
    toH f x y = AlmuH <$> f x y

diffIns :: (IsRecEl u, IsRecEl v, Alternative m, Monad m)
        => Usingl u -> Usingl v -> m (Almu u v)
diffIns x s = case view s of
  (Tag c p) -> Alins c <$> diffInsCtx x p

diffDel :: (IsRecEl u, IsRecEl v, Alternative m, Monad m)
        => Usingl u -> Usingl v -> m (Almu u v)
diffDel s x = case (view s) of
  (Tag c p) -> Aldel c <$> diffDelCtx p x

--- Utility
uncurryPair :: (Monad m)
            => (All Usingl s -> All Usingl d -> m (Al rec s d))
            -> TrivialP s d -> m (Al rec s d)
uncurryPair f (Pair l r) = f l r

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module DiffMultiRec where

import ApplyMultiRec
import MultiRec
import Data.Type.Equality hiding (apply)

diffAt :: (Monad m) => forall rec .
          (forall r . IsRecEl r => Usingl r -> Usingl r -> m (rec r))
       -> Usingl a -> Usingl a -> m (At rec a)
diffAt diffR x@(Uint i)   y@(Uint j) = return $ As $ Contract (x, y)
diffAt diffR x@(Usexpr _) y@(Usexpr _) = Ai <$> diffR x y
diffAt diffR x@(UsexprL _) y@(UsexprL _) = Ai <$> diffR x y

diffS :: (IsRecEl a) => forall rec .
         (forall r . IsRecEl r => Usingl r -> Usingl r -> [rec r])
      -> Usingl a -> Usingl a -> [Spine (At rec) (Al (At rec)) a]
diffS diffR s1 s2 = mapSpineM (uncurry (diffAt diffR) . unContract) (uncurryPair $ alignP diffR) (spine s1 s2)
  where
    alignP :: (forall r . IsRecEl r => Usingl r -> Usingl r -> [rec r])
           -> All Usingl s -> All Usingl d -> [Al (At rec) s d]
    alignP diffR p1 p2 = do
      al <- align p1 p2
      mapAlM (uncurry (diffAt diffR) . unContract) al

diffInsCtx :: (IsRecEl v) => Usingl v -> All Usingl p -> [Ctx (AtmuPos v) p]
diffInsCtx x An                     = []
diffInsCtx x (at@(Uint _)   `Ac` ats) = There at <$> diffInsCtx x ats
diffInsCtx x (at@(Usexpr _) `Ac` ats) =
     (flip Here ats <$> fmap FixPos (diffAlmu x at))
  ++ (There at <$> diffInsCtx x ats)
diffInsCtx x (at@(UsexprL _) `Ac` ats) =
     (flip Here ats <$> fmap FixPos (diffAlmu x at))
  ++ (There at <$> diffInsCtx x ats)

diffDelCtx :: (IsRecEl v) => All Usingl p -> Usingl v -> [Ctx (AtmuNeg v) p]
diffDelCtx An y = []
diffDelCtx (at@(Uint _) `Ac` ats) y = There at <$> diffDelCtx ats y
diffDelCtx (at@(Usexpr _)   `Ac` ats) y =
     (flip Here ats <$> fmap FixNeg (diffAlmu at y))
  ++ (There at <$> diffDelCtx ats y)
diffDelCtx (at@(UsexprL _)   `Ac` ats) y =
     (flip Here ats <$> fmap FixNeg (diffAlmu at y))
  ++ (There at <$> diffDelCtx ats y)
--
diffAlmu :: (IsRecEl u, IsRecEl v) => Usingl u -> Usingl v -> [Almu u v]
diffAlmu x y = diffMod x y ++ diffIns x y ++ diffDel x y
  where
    diffMod :: (IsRecEl u, IsRecEl v) => Usingl u -> Usingl v -> [Almu u v]
    diffMod s1 s2 = case testEquality s1 s2 of
      Just Refl -> Alspn <$> diffS (toH diffAlmu) s1 s2
      Nothing -> []

    toH :: (IsRecEl u) => (Usingl u -> Usingl u -> [Almu u u]) -> Usingl u -> Usingl u -> [AlmuH u]
    toH f x y = AlmuH <$> f x y

    diffIns :: (IsRecEl u, IsRecEl v) =>  Usingl u -> Usingl v -> [Almu u v]
    diffIns x s = case view s of
      (Tag c p) -> Alins c <$> diffInsCtx x p

    diffDel :: (IsRecEl u, IsRecEl v) =>  Usingl u -> Usingl v -> [Almu u v]
    diffDel s x = case (view s) of
      (Tag c p) -> Aldel c <$> diffDelCtx p x

--- Utility
--
uncurryPair :: (All Usingl s -> All Usingl d -> [Al rec s d])
            -> TrivialP s d -> [Al rec s d]
uncurryPair f (Pair l r) = f l r
--
-- almus :: [Almu]
-- almus = diffAlmu (Ui sum1) (Ui sum2)

list = SCons testval (SCons testval SNil )
testval = Value 1
add = Operation testval testval
almus = diffAlmu (UsexprL list) (Usexpr add)

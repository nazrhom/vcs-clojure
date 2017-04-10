{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module DiffMultiRec where

import ApplyMultiRec
import Multirec
import LangRec
import Data.Type.Equality hiding (apply)

diffAt :: (Monad m) => forall rec .
          (forall r . IsRecEl r => Phase -> Usingl r -> Usingl r -> m (rec r))
       -> Phase -> Usingl a -> Usingl a -> m (At rec a)
diffAt f d x = onRecursiveGuy (\y -> Ai <$> (f d x y))
                              (\ny -> return (As (Contract (x , ny))))
    

diffS :: (IsRecEl a) => forall rec .
         (forall r . IsRecEl r => Phase -> Usingl r -> Usingl r -> [rec r])
      -> Phase -> Usingl a -> Usingl a -> [Spine (At rec) (Al (At rec)) a]
diffS diffR d s1 s2
  = mapSpineM (uncurry (diffAt diffR d) . unContract)
              (uncurryPair $ alignP diffR d)
              (spine s1 s2)
  where
    alignP :: (forall r . IsRecEl r => Phase -> Usingl r -> Usingl r -> [rec r])
           -> Phase -> All Usingl s -> All Usingl d -> [Al (At rec) s d]
    alignP diffR d p1 p2 = do
      al <- align p1 p2
      mapAlM (uncurry (diffAt diffR d) . unContract) al

data IsRecGuy (u :: k) :: * where
  IsRecGuy :: (IsRecEl u) => IsRecGuy u

onRecursiveGuy :: ((IsRecEl v) => Usingl v -> a) -> (Usingl v -> a) -> Usingl v -> a
onRecursiveGuy rec nonrec at@(UString _)   = nonrec at
onRecursiveGuy rec nonrec at@(USep _) = rec at
onRecursiveGuy rec nonrec at@(USepExprList _) = rec at
onRecursiveGuy rec nonrec at@(UExpr _) = rec at
onRecursiveGuy rec nonrec at@(UFormTy _) = rec at
onRecursiveGuy rec nonrec at@(UCollType _) = rec at
onRecursiveGuy rec nonrec at@(UTerm _) = rec at
onRecursiveGuy rec nonrec at@(UTag _) = rec at

diffInsCtx :: (IsRecEl v) => Usingl v -> All Usingl p -> [Ctx (AtmuPos v) p]
diffInsCtx x An = []
diffInsCtx x (y `Ac` ay)
  = onRecursiveGuy (rec x ay) (nonrec x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v) => Usingl v -> All Usingl p
        -> Usingl u -> [Ctx (AtmuPos v) (u ': p)]
    rec x ys y = ((\r -> Here (FixPos r) ys) <$> diffAlmu I x y)
              ++ nonrec x ys y

    nonrec :: (IsRecEl v) => Usingl v -> All Usingl p
           -> Usingl u -> [Ctx (AtmuPos v) (u ': p)]
    nonrec x ys y = There y <$> diffInsCtx x ys

diffDelCtx :: (IsRecEl v) => All Usingl p -> Usingl v -> [Ctx (AtmuNeg v) p]
diffDelCtx An x = []
diffDelCtx (y `Ac` ay) x
  = onRecursiveGuy (rec x ay) (nonrec x ay) y
  where
    rec :: (IsRecEl u , IsRecEl v) => Usingl v -> All Usingl p
        -> Usingl u -> [Ctx (AtmuNeg v) (u ': p)]
    rec x ys y = ((\r -> Here (FixNeg r) ys) <$> diffAlmu D y x)
              ++ nonrec x ys y

    nonrec :: (IsRecEl v) => Usingl v -> All Usingl p
           -> Usingl u -> [Ctx (AtmuNeg v) (u ': p)]
    nonrec x ys y = There y <$> diffDelCtx ys x

diffAlmu :: (IsRecEl u, IsRecEl v) => Phase -> Usingl u -> Usingl v -> [Almu u v]
diffAlmu M x y = diffMod x y ++ diffIns x y ++ diffDel x y
diffAlmu I x y = diffMod x y ++ diffIns x y
diffAlmu D x y = diffMod x y ++ diffDel x y

diffMod :: (IsRecEl u, IsRecEl v) => Usingl u -> Usingl v -> [Almu u v]
diffMod s1 s2 = case testEquality s1 s2 of
  Just Refl -> Alspn <$> diffS (\p -> toH (diffAlmu p)) M s1 s2
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

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)


-- TESTS


-- list1 = SCons testval1 (SCons testval2 SNil)
-- list2 = SCons testval2 (SCons testval1 SNil)
-- testval1 = Value 0
-- testval2 = Value 1
-- op1 = Operation testval1 testval2
-- op2 = Operation testval2 testval1
--
-- oplist1 = Operation (List list1) (List list1)
-- listop1 = SCons op1 (SCons op1 SNil)
--
-- oplist2 = Operation (List list1) (List list2)
-- listop2 = SCons op1 (SCons op2 SNil)
--
-- oplist3 = Operation (List listop2) (List listop1)
-- listop3 = SCons oplist1 (SCons oplist2 SNil)
--
-- almus1 = diffAlmu (UsexprL list1) (Usexpr op1)
-- almus2 = diffAlmu (Usexpr oplist1) (UsexprL listop2)
-- almus3 = diffAlmu (Usexpr oplist2) (UsexprL listop2)
-- almus4 = diffAlmu (UsexprL listop3) (Usexpr oplist3)
-- almus5 = diffAlmu (UsexprL list1) (Usexpr oplist1)
-- almus6 = diffAlmu (Usexpr oplist2) (UsexprL listop3)
--
-- patches1 = map (flip applyAlmu (UsexprL list1)) almus1
-- patches2 = map (flip applyAlmu (Usexpr oplist1)) almus2
-- patches3 = map (flip applyAlmu (Usexpr oplist2)) almus3
-- patches4 = map (flip applyAlmu (UsexprL listop3)) almus4
-- patches5 = map (flip applyAlmu (UsexprL list1)) almus5
-- patches6 = map (flip applyAlmu (Usexpr oplist2)) almus6

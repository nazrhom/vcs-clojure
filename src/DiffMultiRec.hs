{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module DiffMultiRec where

import ApplyMultiRec
import MultiRec
import LangRec
import Data.Type.Equality hiding (apply)

diffAt :: (Monad m) => forall rec .
          (forall r . IsRecEl r => Usingl r -> Usingl r -> m (rec r))
       -> Usingl a -> Usingl a -> m (At rec a)
diffAt diffR x@(UString i)   y@(UString j) = return $ As $ Contract (x, y)
diffAt diffR x@(USepExprList _) y@(USepExprList _) = Ai <$> diffR x y
diffAt diffR x@(UExpr _) y@(UExpr _) = Ai <$> diffR x y
diffAt diffR x@(UTerm _) y@(UTerm _) = Ai <$> diffR x y


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
diffInsCtx x (at@(UString _)   `Ac` ats) = There at <$> diffInsCtx x ats
diffInsCtx x (at@(USepExprList _) `Ac` ats) = diffInsHelper x at ats
diffInsCtx x (at@(UExpr _) `Ac` ats) = diffInsHelper x at ats
diffInsCtx x (at@(UTerm _) `Ac` ats) = diffInsHelper x at ats

diffInsHelper :: (IsRecEl v, IsRecEl u) => Usingl v -> Usingl u -> All Usingl p -> [Ctx (AtmuPos v) (u ': p)]
diffInsHelper x at ats =
  (flip Here ats <$> fmap FixPos (diffAlmu x at))
  ++ (There at <$> diffInsCtx x ats)

diffDelCtx :: (IsRecEl v) => All Usingl p -> Usingl v -> [Ctx (AtmuNeg v) p]
diffDelCtx An y = []
diffDelCtx (at@(UString _) `Ac` ats) y = There at <$> diffDelCtx ats y
diffDelCtx (at@(USepExprList _)  `Ac` ats) y = diffDelHelper at ats y
diffDelCtx (at@(UExpr _)  `Ac` ats) y = diffDelHelper at ats y
diffDelCtx (at@(UTerm _) `Ac` ats) y = diffDelHelper at ats y

diffDelHelper :: (IsRecEl v, IsRecEl u) => Usingl u -> All Usingl p -> Usingl v -> [Ctx (AtmuNeg v) (u ': p)]
diffDelHelper at ats y =
  (flip Here ats <$> fmap FixNeg (diffAlmu at y))
  ++ (There at <$> diffDelCtx ats y)


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

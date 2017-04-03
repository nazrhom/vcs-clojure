{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Diff where

import Lang
import Apply
import RegularTypes

diffAt :: Monad m => forall rec . (Usingl KI -> Usingl KI -> m rec)
       -> Usingl a -> Usingl a -> m (At rec a)
diffAt diffR x@(Uint i) y@(Uint j) = return $ As $ Contract (x, y)
diffAt diffR x@(Ui a)   y@(Ui b)   = Ai <$> diffR x y

diffS :: (forall a . Usingl KI -> Usingl KI -> [rec])
      -> Usingl KI -> Usingl KI -> [Spine (At rec) (Al (At rec))]
diffS diffR s1 s2 = mapSpineM (liftContractM $ uncurry (diffAt diffR)) (uncurryPair $ alignP diffR) (spine s1 s2)
  where
    alignP :: (forall a . Usingl KI -> Usingl KI -> [rec])
           -> All Usingl s -> All Usingl d -> [Al (At rec) s d]
    alignP diffR p1 p2 = do
      al <- align p1 p2
      mapAlM (uncurry (diffAt diffR) . unContract) al

--- Utility

liftContractM :: Monad m => forall (p :: U -> *) (q :: U -> *) .
             ((p a, p a) -> m (q a))
             -> Contract p a -> m (Contract q a)
liftContractM f c = do
  r <- f (unContract c)
  return $ mkContract r

mkContract :: forall (p :: U -> *) a . p a -> Contract p a
mkContract r = Contract (r, r)

uncurryPair :: (All Usingl s -> All Usingl d -> [Al rec s d])
            -> TrivialP s d -> [Al rec s d]
uncurryPair f (Pair l r) = f l r

diffCtx :: Usingl KI -> All Usingl p -> [Ctx p]
diffCtx x An                     = []
diffCtx x (at@(Uint i) `Ac` ats) = There at <$> diffCtx x ats
diffCtx x (at@(Ui a)   `Ac` ats) = (flip Here ats <$> diffAlmu x at)
                                ++ (There at <$> diffCtx x ats)

diffAlmu :: Usingl KI -> Usingl KI -> [Almu]
diffAlmu x y = diffMod x y ++ diffIns x y ++ diffDel x y
  where
    diffMod :: Usingl KI -> Usingl KI -> [Almu]
    diffMod s1 s2 = Alspn <$> diffS diffAlmu s1 s2

    diffIns :: Usingl KI -> Usingl KI -> [Almu]
    diffIns x s = case view s of
      (Tag c p) -> Alins c <$> diffCtx x p

    diffDel :: Usingl KI -> Usingl KI -> [Almu]
    diffDel s x = case view s of
      (Tag c p) -> Aldel c <$> diffCtx x p

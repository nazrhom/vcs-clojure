{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Diff where

import Lang
import Apply
import RegularTypes
import Data.Type.Equality hiding (apply)

diffAt :: Monad m => forall rec . (Usingl KI -> Usingl KI -> m rec)
       -> Usingl a -> Usingl a -> m (At rec a)
diffAt diffR x@(Uint i) y@(Uint j) = return $ As $ Contract (x, y)
diffAt diffR x@(Ui a)   y@(Ui b)   = Ai <$> diffR x y

diffS :: forall rec . (Usingl KI -> Usingl KI -> [rec])
      -> Usingl KI -> Usingl KI -> [Spine (At rec) (Al (At rec))]
diffS diffR s1 s2 = mapSpineM (uncurry (diffAt diffR) . unContract) (uncurryPair $ alignP diffR) (spine s1 s2)
  where
    alignP :: (Usingl KI -> Usingl KI -> [rec])
           -> All Usingl s -> All Usingl d -> [Al (At rec) s d]
    alignP diffR p1 p2 = do
      al <- align p1 p2
      mapAlM (uncurry (diffAt diffR) . unContract) al

diffInsCtx :: Usingl KI -> All Usingl p -> [Ctx p]
diffInsCtx x An                     = []
diffInsCtx x (at@(Uint _) `Ac` ats) = There at <$> diffInsCtx x ats
diffInsCtx x (at@(Ui _)   `Ac` ats) = (flip Here ats <$> diffAlmu x at)
                                ++ (There at <$> diffInsCtx x ats)

diffDelCtx :: All Usingl p -> Usingl KI -> [Ctx p]
diffDelCtx An y = []
diffDelCtx (at@(Uint _) `Ac` ats) y = There at <$> diffDelCtx ats y
diffDelCtx (at@(Ui _)   `Ac` ats) y = (flip Here ats <$> diffAlmu at y)
                                ++ (There at <$> diffDelCtx ats y)

diffAlmu :: Usingl KI -> Usingl KI -> [Almu]
diffAlmu x y = diffMod x y ++ diffIns x y ++ diffDel x y
  where
    diffMod :: Usingl KI -> Usingl KI -> [Almu]
    diffMod s1 s2 = Alspn <$> diffS diffAlmu s1 s2

    diffIns :: Usingl KI -> Usingl KI -> [Almu]
    diffIns x s = case view s of
      (Tag c p) -> Alins c <$> diffInsCtx x p

    diffDel :: Usingl KI -> Usingl KI -> [Almu]
    diffDel s x = case (view s) of
      (Tag c p) -> Aldel c <$> diffDelCtx p x
--- Utility

uncurryPair :: (All Usingl s -> All Usingl d -> [Al rec s d])
            -> TrivialP s d -> [Al rec s d]
uncurryPair f (Pair l r) = f l r

-- Test

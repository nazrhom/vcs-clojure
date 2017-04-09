{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}


module ApplyMultiRec where
import Data.Type.Equality hiding (apply)

import MultiRec
import Debug.Trace

applyS :: IsRecEl r => (forall a . at a -> Usingl a -> Maybe (Usingl a))
       -> (forall p1 p2 . al p1 p2 -> All Usingl p1 -> Maybe (All Usingl p2))
       -> Spine at al r
       -> Usingl r
       -> Maybe (Usingl r)
applyS doAt doAl Scp x = Just x
applyS doAt doAl (Schg i j p) x = case view x of
  Tag c d -> case testEquality c i of
    Just Refl -> inj j <$> doAl p d
    Nothing -> Nothing
applyS doAt doAl (Scns i p) x = case view x of
  Tag c d -> case testEquality c i of
    Just Refl -> inj i <$> sAll doAt p d
    Nothing -> Nothing
  where
    sAll :: (forall a . at a -> Usingl a -> Maybe (Usingl a))
         -> All at p -> All Usingl p -> Maybe (All Usingl p)
    sAll doAt An            An          = Just An
    sAll doAt (at `Ac` ats) (a `Ac` as) = do
      a'  <- doAt at a
      as' <- sAll doAt ats as
      return (a' .@. as')

applyAl :: (forall a . at a -> Usingl a -> Maybe (Usingl a))
        -> Al at p1 p2 -> All Usingl p1 -> Maybe (All Usingl p2)
applyAl doAt A0 An = Just An
applyAl doAt (Amod p a) (Ac x xs) = do
  x' <- doAt p x
  xs' <- applyAl doAt a xs
  return (x' .@. xs')
applyAl doAt (Ains k a) xs = do
  ys <- applyAl doAt a xs
  return (k .@. ys)
applyAl doAt (Adel k a) (Ac x xs) = case testEquality x k of
  Just Refl -> applyAl doAt a xs
  Nothing -> trace "applyAl failing" Nothing

applyAt :: (IsRecEl a => rec a -> Usingl a -> Maybe (Usingl a))
        -> At rec a -> Usingl a -> Maybe (Usingl a)
applyAt appRec (Ai r) x = appRec r x
applyAt appRec (As c) x = if old == new then Just x
                          else if old == x then Just new
                          else trace "applyAt failing" Nothing
    where (old, new) = unContract c

applyAtAlmuH :: At AlmuH a -> Usingl a -> Maybe (Usingl a)
applyAtAlmuH = applyAt (unH applyAlmu)
  where
    unH f (AlmuH al) x = f al x

applyAtmuPos :: (IsRecEl u, IsRecEl v) => AtmuPos u v -> Usingl u -> Maybe (Usingl v)
applyAtmuPos (FixPos almu) x = applyAlmu almu x

applyAtmuNeg :: (IsRecEl u, IsRecEl v) => AtmuNeg u v -> Usingl v -> Maybe (Usingl u)
applyAtmuNeg (FixNeg almu) x = applyAlmu almu x

ctxIns :: (forall a . IsRecEl a => p a -> Maybe (Usingl a)) -> Ctx p l -> Maybe (All Usingl l)
ctxIns f (There x xs) = Ac x <$> ctxIns f xs
ctxIns f (Here x xs) = do
  x' <- f x
  return $ x' .@. xs

ctxDel :: IsRecEl v => Ctx (AtmuNeg v) l -> All Usingl l -> Maybe (Usingl v)
ctxDel (Here px _)  (xx `Ac` _)  = applyAtmuNeg px xx
ctxDel (There _ xs) (_ `Ac` xss) = ctxDel xs xss

applyAlmu :: (IsRecEl u, IsRecEl v) => Almu u v -> Usingl u -> Maybe (Usingl v)
applyAlmu (Alspn s) x = applyS applyAtAlmuH (applyAl applyAtAlmuH) s x
applyAlmu (Alins constr ctx) x = inj constr <$> ctxIns (\p -> applyAtmuPos p x) ctx
applyAlmu (Aldel constr ctx) x = case view x of
  (Tag c1 p1) -> case testEquality constr c1 of
    Just Refl -> ctxDel ctx p1
    Nothing -> trace ("applyAlmu failing.")  Nothing
--
-- insCtx :: Ctx p -> Usingl KI -> Maybe (All Usingl p)
-- insCtx (There atmu ctx) x = Ac atmu <$> insCtx ctx x
-- insCtx (Here spmu atmus) x = do
--   h <- applyAlmu spmu x
--   return (h .@. atmus)
--
-- delCtx :: Ctx p -> All Usingl p -> Maybe (Usingl KI)
-- delCtx (Here  spmu atmus) (x  `Ac` p) = applyAlmu spmu x
-- delCtx (There atmu ctx)   (at `Ac` p) = delCtx ctx p

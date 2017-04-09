{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}


module Apply where

import Data.Type.Equality hiding (apply)

import RegularTypes
import Lang
import Debug.Trace

applyS :: (forall a . at a -> Usingl a -> Maybe (Usingl a))
       -> (forall p1 p2 . al p1 p2 -> All Usingl p1 -> Maybe (All Usingl p2))
       -> Spine at al
       -> Usingl KI
       -> Maybe (Usingl KI)
applyS doAt doAl Scp x = Just x
applyS doAt doAl (Schg i j p) x = case view x of
  (Tag c d) -> case testEquality c i of
    Just Refl -> inj j <$> doAl p d
    Nothing -> trace ("applyS failing Schg" ++ show c ++ " does not match " ++ show i) Nothing
applyS doAt doAl (Scns i p) x = case view x of
  (Tag c d) -> case testEquality c i of
    Just Refl -> inj i <$> sAll doAt p d
    Nothing -> trace "applyS failing Scns" Nothing
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

applyAt :: (rec -> Usingl KI -> Maybe (Usingl KI))
        -> At rec a -> Usingl a -> Maybe (Usingl a)
applyAt appRec (Ai r) x = appRec r x
applyAt appRec (As c) x = if old == new then Just x
                          else if old == x then Just new
                          else trace "applyAt failing" Nothing
    where (old, new) = unContract c

applyAtmu :: At Almu a -> Usingl a -> Maybe (Usingl a)
applyAtmu = applyAt applyAlmu

applyAlmu :: Almu -> Usingl KI -> Maybe (Usingl KI)
applyAlmu (Alspn s)          x = applyS applyAtmu (applyAl applyAtmu) s x
applyAlmu (Alins constr ctx) x = inj constr <$> insCtx ctx x
applyAlmu (Aldel constr ctx) x = case view x of
  (Tag c1 p1) -> case testEquality constr c1 of
    Just Refl -> delCtx ctx p1
    Nothing -> trace ("applyAlmu failing. Trying to delete " ++ show c1  ++ " from " ++ show constr)  Nothing

insCtx :: Ctx p -> Usingl KI -> Maybe (All Usingl p)
insCtx (There atmu ctx) x = Ac atmu <$> insCtx ctx x
insCtx (Here spmu atmus) x = do
  h <- applyAlmu spmu x
  return (h .@. atmus)

delCtx :: Ctx p -> All Usingl p -> Maybe (Usingl KI)
delCtx (Here  spmu atmus) (x  `Ac` p) = applyAlmu spmu x
delCtx (There atmu ctx)   (at `Ac` p) = delCtx ctx p

inj :: SExprConstr c -> All Usingl (TypeOf c) -> Usingl KI
inj S_Add    (x `Ac` y `Ac` An) = Ui (Add (eval x) (eval y))
inj S_Square (x `Ac` An)        = Ui (Square (eval x))
inj S_Value  (x `Ac` An)        = Ui (Value (eval x))

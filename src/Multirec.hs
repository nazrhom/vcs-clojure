{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Multirec where

import Data.Kind
import Data.Type.Equality hiding (apply)
import Control.Applicative
import qualified Data.Map  as M

import Parser
import Lang

-- The actual puzzle

data Spine (at :: U -> *)(al :: [U] -> [U] -> *) :: U -> * where
  Scp  :: Spine at al u
  Scns :: ConstrFor u s -> All at (TypeOf s) -> Spine at al u
  Schg :: ConstrFor u s -> ConstrFor u r
       -> al (TypeOf s) (TypeOf r)
       -> Spine at al u

data Al (at :: U -> *) :: [U] -> [U] -> * where
  A0   :: Al at '[] '[]
  Ains :: Usingl u -> Al at xs ys -> Al at xs (u ': ys)
  Adel :: Usingl u -> Al at xs ys -> Al at (u ': xs) ys
  Amod :: at u -> Al at xs ys -> Al at (u ': xs) (u ': ys)

data At (recP :: U -> *) :: U -> * where
  Ai :: (IsRecEl u) => recP u -> At recP u
  As :: TrivialA u -> At recP u

data Almu :: U -> U -> * where
  Alspn :: Spine (At AlmuH) (Al (At AlmuH)) u -> Almu u u
  Alins :: ConstrFor v s -> Ctx (AtmuPos u) (TypeOf s) -> Almu u v
  Aldel :: ConstrFor u s -> Ctx (AtmuNeg v) (TypeOf s) -> Almu u v

data AlmuH :: U -> * where
  AlmuH :: Almu u u -> AlmuH u
  deriving Show

unH :: AlmuH u -> Almu u u
unH (AlmuH u) = u

-- Atmu positive and negative variations
data AtmuPos (v :: U) :: U -> * where
  FixPos :: Almu v u -> AtmuPos v u
  deriving Show

unPos :: AtmuPos v u -> Almu v u
unPos (FixPos p) = p

data AtmuNeg (v :: U) :: U -> * where
  FixNeg :: Almu u v -> AtmuNeg v u
  deriving Show

unNeg :: AtmuNeg u v -> Almu v u
unNeg (FixNeg n) = n

data Ctx (r :: U -> *) :: [U] -> * where
  Here :: (IsRecEl u) => r u -> All Usingl l  -> Ctx r (u ': l)
  There :: Usingl u -> Ctx r l -> Ctx r (u ': l)

spine :: IsRecEl r => Usingl r -> Usingl r -> Spine TrivialA TrivialP r
spine x y | x == y = Scp
spine x y | otherwise = case (view x, view y) of
  ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
    Just Refl -> Scns c1 (zipP l1 l2)
    Nothing -> Schg c1 c2 (Pair l1 l2)

-- |Implement DP-style optimization for Alignments and Recursive alignmetns.
--
-- The phase records the LAST decision took by the algo (either an insertion,
-- modification ordeletion)
data Phase
  = I | M | D
  deriving (Eq , Show)

align :: (Alternative m) => All Usingl p1 -> All Usingl p2 -> m (Al TrivialA p1 p2)
align = alignOpt M


shouldAlign :: (Alternative m)
            => Usingl a1 -> Usingl a2 -> m (Al TrivialA p1 p2)
            -> m (Al TrivialA (a1 ': p1) (a2 ': p2))
shouldAlign at1 at2 rest
  = case testEquality at1 at2 of
      Just Refl -> Amod (Contract (at1, at2)) <$> rest
      Nothing   -> empty

{- The optimization works as follow: For each step in which we could perform a match, we attempt an insertion aswell. However this insertion only makes sense if it leads to either another match, or to no more deletions.
Infact, if at any point of this chain we perform a deletion, we will end up in the same state that we would have been if we just performed the original match at the beginning. -}
alignOpt :: (Alternative m) => Phase -> All Usingl p1 -> All Usingl p2 -> m (Al TrivialA p1 p2)
alignOpt _ An           An          = pure A0

-- alignOp D == align-no-ins
alignOpt D An           (b `Ac` pb) = empty
alignOpt D (a `Ac` pa)  (b `Ac` pb) = shouldAlign a b (align pa pb)
                            <|> Adel a <$> alignOpt D pa (b `Ac` pb)
-- alignOpt I == align-no-del
alignOpt I (a `Ac` pa)  An          = empty
alignOpt I (a `Ac` pa)  (b `Ac` pb) = shouldAlign a b (align pa pb)
                            <|> Ains b <$> alignOpt I (a `Ac` pa) pb
-- alignOpt M == align*
alignOpt M An           (b `Ac` pb) = Ains b <$> align An pb
alignOpt M (a `Ac` pa)  An          = Adel a <$> align pa An
alignOpt M (a `Ac` pa)  (b `Ac` pb) = case testEquality a b of
  Just Refl -> Amod (Contract (a, b)) <$> align pa pb
           <|> Ains b <$> alignOpt I (a .@. pa) pb
           <|> Adel a <$> alignOpt D pa (b .@. pb)
  Nothing   -> Ains b <$> alignOpt I (a .@. pa) pb
           <|> Adel a <$> align pa (b .@. pb)


-- Library stuff
newtype Contract (f :: k -> *) (x :: k) = Contract { unContract :: (f x , f x) }

type TrivialA = Contract Usingl
data TrivialP :: [U] -> [U] -> * where
 Pair :: All Usingl l -> All Usingl r -> TrivialP l r

mapAll :: (forall a . p a -> q a) -> All p l -> All q l
mapAll f An = An
mapAll f (a `Ac` as) = f a `Ac` mapAll f as

foldAll :: (forall a . p a -> b -> b) -> b -> All p a -> b
foldAll f b An          = b
foldAll f b (a `Ac` as) = f a (foldAll f b as)

foldCtx :: (forall u . r u -> b -> b)
        -> (forall u . Usingl u -> b -> b)
        -> b -> Ctx r l -> b
foldCtx f g b (Here r p)  = f r (foldAll g b p)
foldCtx f g b (There u c) = foldCtx f g b c

mapAllM :: Monad m => (forall a . p a -> m (q a))
        -> All p xs -> m (All q xs)
mapAllM f An = return An
mapAllM f (px `Ac` pxs) = Ac <$> f px <*> mapAllM f pxs

zipP :: All p a -> All p a -> All (Contract p) a
zipP An          An          = An
zipP (a `Ac` as) (b `Ac` bs) = Contract (a, b) .@. zipP as bs

mapSpineM :: Monad m => (forall a . at1 a -> m (at2 a))
     -> (forall s d . al1 s d -> m (al2 s d))
     -> Spine at1 al1 u -> m (Spine at2 al2 u)
mapSpineM f g Scp             = return Scp
mapSpineM f g (Scns c ps)     = Scns c <$> mapAllM f ps
mapSpineM f g (Schg c1 c2 al) = Schg c1 c2 <$> g al

mapAlM :: Monad m => (forall a . at1 a -> m (at2 a))
          -> Al at1 s d -> m (Al at2 s d)
mapAlM f A0           = return A0
mapAlM f (Adel at al) = Adel at <$> mapAlM f al
mapAlM f (Ains at al) = Ains at <$> mapAlM f al
mapAlM f (Amod at al) = Amod    <$> f at <*> mapAlM f al

mapAt :: (forall a . rec1 a -> rec2 a)
      -> At rec1 a -> At rec2 a
mapAt f (Ai r) = Ai (f r)
mapAt f (As t) = As t
-- Show instances
instance Show (Almu u v) where
  show = showAlmu
instance Show (Al at p1 p2) where
  show = showAl
instance Show (At AlmuH u) where
  show = showAt
instance Show (Spine (At AlmuH) al u) where
  show = showSpine
instance Show (All (At AlmuH) l) where
  show = showAll
instance Show (Ctx (AtmuPos u) p) where
  show = showCtxP
instance Show (Ctx (AtmuNeg u) p) where
  show = showCtxN
instance Show (f x) => Show (Contract f x) where
  show c = show $ unContract c

showPatchEffect :: (IsRecEl u, IsRecEl v) => Almu u v -> Usingl u ->  String
showPatchEffect (Alspn s) e =
  showSpineEffect
    (showAtEffect showAlmuHEffect)
    (showAlEffect (showAtEffect showAlmuHEffect))
    e
    s
showPatchEffect (Alins c ctx) e = "\n{+" ++ showConstr c ++  showCtxPosE ctx e
showPatchEffect (Aldel c ctx) e = case view e of
  (Tag c' d) -> case testEquality' c c' of
    Nothing -> "error"
    Just (Refl, Refl) -> "\n{-" ++ showConstr c ++ showCtxNegE ctx d


showCtxPosE :: IsRecEl u => Ctx (AtmuPos u) l -> Usingl u -> String
showCtxPosE (Here r p) u = "+}\n" ++ showAtmuPosE u r
showCtxPosE (There u' ctx) u = show u' ++ showCtxPosE ctx u

showCtxNegE :: IsRecEl v => Ctx (AtmuNeg v) l -> All Usingl l -> String
showCtxNegE (Here r p) (u `Ac` us) = "-}\n" ++ showAtmuNegE u r
showCtxNegE (There u' ctx) (u `Ac` us) = show u' ++ showCtxNegE ctx us

showSpineEffect :: IsRecEl u => (forall u . Usingl u -> at u -> String)
                -> (forall p1 p2 . All Usingl p1 -> al p1 p2 -> String)
                -> Usingl u -> Spine at al u -> String
showSpineEffect showAtE showAlE e Scp = show e
showSpineEffect showAtE showAlE e (Scns i p) = case view e of
  (Tag c d) -> case testEquality c i of
    Nothing -> undefined
    Just Refl -> showConstr c ++ showAll showAtE d p
      where
        showAll :: (forall u . Usingl u -> at u -> String)
                  -> All Usingl l -> All (at :: U -> *) l -> String
        showAll showAtE An An = ""
        showAll showAtE (e `Ac` es) (at `Ac` ats) = showAtE e at ++ showAll showAtE es ats
showSpineEffect showAtE showAlE e (Schg i j p) = case view e of
  (Tag c d) -> case testEquality c i of
    Just Refl ->
      "(" ++ showConstr i ++ " -> " ++ showConstr j ++ ")" ++ showAlE d p
    Nothing -> undefined

showAlEffect :: (forall u . Usingl u -> at u -> String)
             -> All Usingl p1 -> Al at p1 p2  -> String
showAlEffect showAtE An A0 = ""
showAlEffect showAtE (u `Ac` us) (Amod u' al) = "[%" ++ showAtE u u' ++ "%]" ++ showAlEffect showAtE us al
showAlEffect showAtE (u `Ac` us) (Adel u' al) = "[-" ++ showPair u u' ++ "-]" ++ showAlEffect showAtE us al
showAlEffect showAtE (u `Ac` us) (Ains u' al) = "[+" ++ show u ++ "+]" ++ showAlEffect showAtE (u .@. us) al

showPair :: Usingl u -> Usingl v -> String
showPair u v = show u ++ ", " ++ show v

showAtEffect :: (IsRecEl u => Usingl u -> rec u -> String)
              -> Usingl u -> At rec u -> String
showAtEffect showREffect u (As pair) = show pair
showAtEffect showREffect u (Ai spmu) = showREffect u spmu

showAlmuHEffect :: IsRecEl u => Usingl u -> AlmuH u -> String
showAlmuHEffect u (AlmuH almu) = showPatchEffect almu u

showAtmuPosE :: (IsRecEl u, IsRecEl v) => Usingl u -> AtmuPos u v -> String
showAtmuPosE u (FixPos almu) = showPatchEffect almu u

showAtmuNegE :: (IsRecEl u, IsRecEl v) => Usingl v -> AtmuNeg u v -> String
showAtmuNegE u (FixNeg almu) = showPatchEffect almu u

showAll :: All (At AlmuH) l -> String
showAll An = ""
showAll (x `Ac` xs) = show x ++ show xs

showSpine :: Spine (At AlmuH) al u -> String
showSpine (Scp) = "Scp. "
showSpine (Scns i p) = "Scns " ++ show i ++ " (" ++ show p ++ ") "
showSpine (Schg i j p) = "Schg From: " ++ show i ++ " to " ++ show j ++ "{missing almu}"

showAlmu :: Almu u v -> String
showAlmu (Alspn s) = "M-" ++ show s
showAlmu (Alins c d) = "I-" ++ show d
showAlmu (Aldel c d) = "D-" ++ show d

showCtxP :: Ctx (AtmuPos u) p -> String
showCtxP (Here r p) = show r
showCtxP (There u c) = show c

showCtxN :: Ctx (AtmuNeg u) p -> String
showCtxN (Here r p) = show r
showCtxN (There u c) = show c

showAt :: At AlmuH u -> String
showAt (Ai r) = show r
showAt (As t) = show t

showAl :: Al at p1 p2 -> String
showAl A0 = "0"
showAl (Ains _ al) = '+' : showAl al
showAl (Adel _ al) = '-' : showAl al
showAl (Amod _ al) = '%' : showAl al

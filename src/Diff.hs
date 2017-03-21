{-#LANGUAGE GADTs, DataKinds, TypeOperators, PolyKinds,  StandaloneDeriving, RankNTypes, FlexibleContexts, FlexibleInstances, TypeFamilyDependencies, ScopedTypeVariables, ConstraintKinds, UndecidableInstances, TypeInType#-}
module Diff where

import Data.Kind
import GHC.Exts
import Data.Typeable
import Data.Type.Equality hiding (apply)

import HList
import Lang

--- SPINE

data View s where
  Tag :: SSEC c -> HList (TypeOf c) -> View SExpr

instance Show (View s) where
  show (Tag S_Add xs) = "(S_Add " ++ show xs ++ ")"
  show (Tag S_Square xs) = "(S_Square " ++ show xs ++ ")"
  show (Tag S_Value xs) = "(S_Value " ++ show xs ++ ")"

view :: SExpr -> View SExpr
view (Add a b) = Tag S_Add (a .*. b .*. HNil)
view (Square a) = Tag S_Square (a .*. HNil)
view (Value v) = Tag S_Value (v .*. HNil)

inj :: SSEC c -> HList (TypeOf c) -> SExpr
inj S_Add (x `HCons` y `HCons` HNil) = Add x y
inj S_Square (x `HCons` HNil) = Square x
inj S_Value (x `HCons` HNil) = Value x

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance All c '[] = ()
type instance All c (x ': xs) = (c x, All c xs)

type family AllL p i where
  AllL p (x:xs) = (p x x) : (AllL p xs)
  AllL p '[]    = '[]

data SExprSpine (k :: * -> * -> *) (p :: [*] -> [*] -> *) where
  SScp :: SExprSpine k p
  SSCns :: SSEC i -> HList (AllL k (TypeOf i)) -> SExprSpine k p
  SSChg :: SSEC i -> SSEC j -> p (TypeOf i) (TypeOf j) -> SExprSpine k p

instance Show (SExprSpine k p) where
  show SScp = "Copy"
  show (SSCns i p) = "Cns " ++ show i
  show (SSChg i j p) = "Chg " ++ show i ++ " " ++ show j

spine :: SExpr -> SExpr -> SExprSpine TrivialA TrivialP
spine x y | x == y = SScp
           | otherwise = case (view x, view y) of
             ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
               Just Refl -> SSCns c1 (allLTrivialA l1)
               Nothing -> SSChg c1 c2 (trivialP l1 l2)

applyS :: (forall e . k e e -> e -> Maybe e) ->
          (forall s d . p s d -> HList s -> Maybe (HList d)) ->
          SExprSpine k p ->
          SExpr ->
          Maybe SExpr
applyS doK doP SScp x = Just x
applyS doK doP (SSChg i j p) x = case view x of
  (Tag c d) -> case testEquality c i of
    Just Refl -> inj j <$> doP p d
    Nothing -> Nothing
applyS doK doP (SSCns i ps) x = case view x of
  (Tag c d) -> case testEquality c i of
    Just Refl -> inj i <$> sAll doK ps d
    Nothing -> Nothing
  where
    sAll :: (forall e . k e e -> e -> Maybe e) ->
            HList (AllL k l) -> HList l -> Maybe (HList l)
    sAll doK HNil HNil = Just HNil
    sAll doK (HCons a as) (HCons b bs) = do
      k <- doK a b
      ks <- sAll doK as bs
      return (k .*. ks)

-- ALIGNMENT

-- TODO: Make trivial polykinded?
data TrivialA a b where
  AL :: a -> b -> TrivialA a b
  deriving Show

data TrivialP a b where
  PL :: HList a -> HList b -> TrivialP a b

deriving instance (Show (HList a), Show (HList b)) => Show (TrivialP a b)

trivialP :: HList a -> HList b -> TrivialP a b
trivialP a b = PL a b

trivialA :: a -> b -> TrivialA a b
trivialA a b = AL a b


allLTrivialA :: HList l -> HList (AllL TrivialA l)
allLTrivialA (HCons a b) = (trivialA a a) `HCons` (allLTrivialA b)
allLTrivialA HNil = HNil

data AI (k :: * -> * -> *) s d where
  A0 :: AI k '[] '[]
  Adel :: a  -> AI k s d -> AI k (a : s) d
  Ains :: a -> AI k s d -> AI k s (a : d)
  AX :: k a b -> AI k s d -> AI k (a : s) (b : d)

-- instance TestEquality Eq where
--   testEquality a b = if a == b then Just Refl else Nothing
--
-- align :: (All Eq a, All Eq b) => HList a -> HList b -> [AI TrivialA a b]
-- align HNil HNil = return A0
-- align HNil (HCons b bs) = Ains b <$> align HNil bs
-- align (HCons a as) HNil = Adel a <$> align as HNil
-- align x@(HCons a as) y@(HCons b bs) = case a == b of
--   Just Refl -> (AX (AL a b) <$> align as bs) ++ (Adel a <$> align as (b .*. bs)) ++ (Ains b <$> align (a .*. as) bs)
--   Nothing -> (Adel a <$> align as (b .*. bs)) ++ (Ains b <$> align (a .*. as) bs)

align :: AtomList a -> AtomList b -> [AI TrivialA a b]
align ANil ANil = return A0
align ANil (ACons b sb bs) = Ains (evalA b) <$> align ANil bs
align (ACons a sa as) ANil = Adel (evalA a) <$> align as ANil
align x@(ACons a sa as) y@(ACons b sb bs) = case testEquality sa sb of
  Just Refl -> (AX (AL (evalA a) (evalA b)) <$> align as bs) ++ (Adel (evalA a) <$> align as (ACons b sb bs)) ++ (Ains (evalA b) <$> align (ACons a sa as) bs)
  Nothing -> (Adel (evalA a) <$> align as (ACons b sb bs)) ++ (Ains (evalA b) <$> align (ACons a sa as) bs)


applyAI :: (All Eq s, All Eq d) => (forall a b . k a b -> a -> Maybe b) ->
           AI k s d -> HList s -> Maybe (HList d)
applyAI doK A0 HNil = Just HNil
applyAI doK (AX p a) (HCons x xs) = do
  x' <- doK p x
  xs' <- applyAI doK a xs
  return (x' .*. xs')
applyAI doK (Ains k a) xs = do
  ys <- applyAI doK a xs
  return (k .*. ys)
applyAI doK (Adel k a) (HCons x xs) =
  if x == k
    then applyAI doK a xs
    else Nothing

--- Patch

type family Patch a where
  Patch a = (a, a)
-- data Patch a where
--   Patch :: a -> a -> Patch (a, a)

diff :: a -> a -> Patch a
diff x y = (x, y)

apply :: (Eq a) => Patch a -> a -> Maybe a
apply (x, y) z = if x == y then Just z
                 else if x == z then Just y
                 else Nothing

-- data A where
--   AAI :: A
--   AAK :: Int -> A
--
-- type family EA a x where
--   EA AAI     x = x
--   EA (AAK k) x = k
--
-- data AList h :: * where
--   ALNil :: AList '[]
--   ALCons :: SA a -> AList l -> AList (a ': l)
--
-- data SA a where
--   SAAI :: SA 'AAI
--   SAAK :: a -> SA ('AAK a)
--
-- instance TestEquality SA where
--   testEquality SAAI SAAI = Just Refl
--   testEquality (SAAK x) (SAAK y) = if x == y then Just Refl else Nothing
--   testEquality _ _ = Nothing
-- data AAt p a where
--   AFix :: p -> AAt p AAI
--   ASet :: TrivialK k -> AAt p (AAK k)

-- applyAT :: (p -> a -> Maybe a) -> AAt p a -> SA a -> Maybe (SA a)
-- applyAT (Set k) x = applyK k x
-- applyAT doP (Fix p) x = doP p x
-- Atoms
data Atom a where
  KInt :: Int -> Atom Int
  I :: x -> Atom x

data AtomList h :: * where
  ANil :: AtomList '[]
  ACons :: Atom a -> SAtom b -> AtomList l -> AtomList (a ': l)

data SAtom a where
  SI :: SAtom 'I
  SKInt :: Int -> SAtom 'KInt

type family EvalA a where
  EvalA (KInt k) = k
  EvalA (I x) = x

evalA :: Atom a -> a
evalA (KInt k) = k
evalA (I x) = x

instance TestEquality SAtom where
  testEquality = sAtomEq

sAtomEq :: SAtom a -> SAtom b -> Maybe (a :~: b)
sAtomEq SI SI = Just Refl
sAtomEq (SKInt x) (SKInt y) = if x == y then Just Refl else Nothing
sAtomEq _ _ = Nothing

-- sAtomEq
data At p (a :: *) (b :: *) where
  Fix :: p -> At p a b
  Set :: TrivialK k -> At p Int Int

type family TrivialK k where
  TrivialK k = Patch Int

applyK :: TrivialK k -> Int -> Maybe Int
applyK = apply
--
applyAT :: (forall a b . p -> a -> Maybe b) -> At p a b -> a -> Maybe b
applyAT doP (Fix p) x = doP p x
applyAT doP (Set k) x = applyK k x
--
data ALMu where
  Spn :: SExprSpine ATMu (AI ATMu) -> ALMu
  Ins :: (SSEC i) -> Ctx (TypeOf i) -> ALMu
  Del :: (SSEC j) -> Ctx (TypeOf j) -> ALMu

type family ATMu :: * -> * -> * where
  ATMu = At ALMu

data Ctx e where
  -- Here :: ALMu -> HList (AllL p a) -> Ctx (I p ': a)
  There :: a -> Ctx p -> Ctx (a ': p)

-- applyAtMu = applyAT applyAlMu
--
-- applyAlMu :: ALMu -> a -> Maybe b
-- applyAlMu (Spn s) x = applyS applyAtMu (applyAI applyAtMu) s x
-- insCtx :: Ctx p -> SExpr -> Maybe b
-- insCtx (There atMu d) x = atMu
-- Test
a = Value (IVal 1)
b = Value (IVal 1)
c = Value (IVal 2)
d = Value (IVal 2)
sum1 = Add a b
sum2 = Add c d
square1 = Square a

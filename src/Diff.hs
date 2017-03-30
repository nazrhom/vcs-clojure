{-#LANGUAGE GADTs, DataKinds, TypeOperators, PolyKinds,  StandaloneDeriving, RankNTypes, FlexibleContexts, FlexibleInstances, TypeFamilyDependencies, ScopedTypeVariables, ConstraintKinds, UndecidableInstances, TypeInType#-}
module Diff where

import Data.Kind
import GHC.Exts
import Data.Typeable
import Data.Type.Equality hiding (apply)

import Data.Maybe
import HList
import Lang

--- SPINE

data View s where
  Tag :: SSEC c -> AtomList s (TypeOf c) -> View s

instance Show (View s) where
  show (Tag S_Add xs) = "(S_Add " ++ show xs ++ ")"
  show (Tag S_Square xs) = "(S_Square " ++ show xs ++ ")"
  show (Tag S_Value xs) = "(S_Value " ++ show xs ++ ")"

class Viewable a where
  view :: a -> View a


instance Viewable SExpr where
  view = viewS

viewS :: SExpr -> View SExpr
viewS (Add a b) = Tag S_Add (I a .@. I b  .@. ANil)
viewS (Square a) = Tag S_Square (I a .@. ANil)
viewS (Value v) = Tag S_Value (KInt v .@. ANil)

inj :: SSEC c -> AtomList a (TypeOf c) -> a
inj = undefined
-- inj c l = inj' c (toHList l)

-- inj'' :: View a -> a
-- inj'' (Tag c d) = inj c d
-- inj S_Add l = inj' S_Add (toHList l)
-- inj S_Square l = inj' S_Square (toHList l)
-- inj S_Value l = inj' S_Value (toHList l)
--
inj' :: SSEC c -> HList (TypeOf c) -> SExpr
inj' S_Add (x `HCons` y `HCons` HNil) = Add x y
inj' S_Square (x `HCons` HNil) = Square x
inj' S_Value (x `HCons` HNil) = Value x

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint
type instance All c '[] = ()
type instance All c (x ': xs) = (c x, All c xs)

data AllL (k :: * -> * -> *) (s :: [*]) where
  None :: AllL k '[]
  Chain :: k x x -> AllL k xs -> AllL k (x ': xs)

data SExprSpine (k :: * -> * -> *) (p :: [*] -> [*] -> *) where
  SScp :: SExprSpine k p
  SSCns :: SSEC i -> AllL k (TypeOf i) -> SExprSpine k p
  SSChg :: SSEC i -> SSEC j -> p (TypeOf i) (TypeOf j) -> SExprSpine k p

instance Show (SExprSpine k p) where
  show SScp = "Copy"
  show (SSCns i p) = "Cns " ++ show i
  show (SSChg i j p) = "Chg " ++ show i ++ " " ++ show j

spine :: (Viewable a, Eq a) => a -> a -> SExprSpine TrivialA TrivialP
spine x y | x == y = SScp
           | otherwise = case (view x, view y) of
             ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
               Just Refl -> SSCns c1 (zipAList l1 l2)
               Nothing -> SSChg c1 c2 (trivialP (toHList l1) (toHList l2))

applyS :: Viewable a => (forall e . k e e -> Atom a e -> Maybe (Atom a e)) ->
          (forall s d . p s d -> AtomList a s -> Maybe (AtomList a d)) ->
          SExprSpine k p ->
          a ->
          Maybe a
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
    sAll :: (forall e . k e e -> Atom a e -> Maybe (Atom a e)) ->
            (AllL k l) -> AtomList a l -> Maybe (AtomList a l)
    sAll doK None ANil = Just ANil
    sAll doK (Chain a as) (ACons b bs) = do
      k <- doK a b
      ks <- sAll doK as bs
      return (k .@. ks)

-- ALIGNMENT

data TrivialA a b where
  TA :: a -> b -> TrivialA a b
  deriving Show

data TrivialP a b where
  TP :: HList a -> HList b -> TrivialP a b

deriving instance (Show (HList a), Show (HList b)) => Show (TrivialP a b)

trivialP :: HList a -> HList b -> TrivialP a b
trivialP a b = TP a b

trivialA :: a -> b -> TrivialA a b
trivialA a b = TA a b

zipAList :: AtomList rec a -> AtomList rec a -> (AllL TrivialA a)
zipAList ANil ANil = None
zipAList (ACons a as) (ACons b bs) = (trivialA (evalA a) (evalA b)) `Chain` (zipAList as bs)

data Al (rec :: *) (k :: * -> * -> *) s d where
  A0 :: Al rec k '[] '[]
  Adel :: Atom rec a -> Al rec k s d -> Al rec k (a : s) d
  Ains :: Atom rec a -> Al rec k s d -> Al rec k s (a : d)
  AX :: k a a -> Al rec k s d -> Al rec k (a : s) (a : d)

align :: AtomList a h -> AtomList a h' -> [Al a TrivialA h h']
align ANil ANil = return A0
align ANil (ACons b bs) = Ains b <$> align ANil bs
align (ACons a as) ANil = Adel a <$> align as ANil
align (ACons a as) (ACons b bs) = case testEquality a b of
  Just Refl -> (AX (TA (evalA a) (evalA b)) <$> align as bs)
            ++ (Adel a <$> align as (ACons b bs))
            ++ (Ains b <$> align (ACons a as) bs)
  Nothing -> (Adel a <$> align as (ACons b bs))
          ++ (Ains b <$> align (ACons a as) bs)


applyAl :: (forall e . k e e -> Atom a e -> Maybe (Atom a e)) ->
           Al a k s d -> AtomList a s -> Maybe (AtomList a d)
applyAl doK A0 ANil = Just ANil
applyAl doK (AX p a) (ACons x xs) = do
  x' <- doK p x
  xs' <- applyAl doK a xs
  return (x' `ACons` xs')
applyAl doK (Ains k a) xs = do
  ys <- applyAl doK a xs
  return (k `ACons` ys)
applyAl doK (Adel k a) (ACons x xs) = case testEquality x k of
  Just Refl -> applyAl doK a xs
  Nothing -> Nothing
--- Patch

data Patch a = Trivial (a, a)

diff :: a -> a -> Patch a
diff x y = Trivial (x, y)

apply :: (Eq a) => Patch a -> a -> Maybe a
apply (Trivial (x, y)) z = if x == y then Just z
                 else if x == z then Just y
                 else Nothing

-- Atoms
data Atom a b where
  KInt :: Int -> Atom a Int
  I :: a -> Atom a a

evalA :: Atom a b -> b
evalA (KInt i) = i
evalA (I b) = b

data AtomList a h :: * where
  ANil :: AtomList a '[]
  ACons :: Atom a b -> AtomList a l -> AtomList a (b ': l)

(.@.) :: Atom a b -> AtomList a l -> AtomList a (b ': l)
(.@.) = ACons
infixr 2 .@.
infixr 2 `ACons`

type SEAtom a = AtomList SExpr a

toHList :: AtomList a b -> HList b
toHList ANil = HNil
toHList (a `ACons` b) = (evalA a) `HCons` toHList b

instance TestEquality (Atom a) where
  testEquality = eqAtom

instance Show b => Show (Atom a b) where
  show (KInt k) = show k
  show (I a)    = show a

instance Show (AtomList a '[]) where
  show ANil = "[]"

instance (Show (Atom a e), Show (AtomList a l)) => Show (AtomList a (e : l)) where
  show (ACons x ANil) = show x ++ "]"
  show (ACons x xs) = "[" ++ show x ++ ", " ++ show xs

eqAtom :: Atom a b -> Atom a c -> Maybe (b :~: c)
eqAtom (KInt i) (KInt j) = if i == j then Just Refl else Nothing
eqAtom (I a) (I b) = Just Refl
eqAtom _ _ = Nothing

-- sAtomEq
data At p a b where
  Fix :: p -> At p a a
  Set :: Patch Int -> At p k k


applyK :: Patch Int -> Int -> Maybe Int
applyK = apply
--
applyAT :: (p -> Atom rec a -> Maybe (Atom rec a)) -> At p a a -> Atom rec a -> Maybe (Atom rec a)
applyAT doP (Fix p) x = doP p x
applyAT doP (Set k) (KInt x) = fmap KInt $ applyK k x
applyAT _ _ _ = Nothing
--
data ALMu ty where
  Spn :: SExprSpine (ATMu ty) (Al ty (ATMu ty)) -> ALMu ty
  Ins :: (SSEC i) -> Ctx (TypeOf i) -> ALMu ty
  Del :: (SSEC j) -> Ctx (TypeOf j) -> ALMu ty

type ATMu ty = At (ALMu ty)

data Ctx e where
  Here :: ALMu ty -> AllL k l -> Ctx (a ': l)
  There :: Atom rec a -> Ctx p -> Ctx (a ': p)

matchCtx :: Ctx c -> AtomList SExpr as -> Maybe (AtomList SExpr bs)
matchCtx (There atmu al) (ACons a as) = matchCtx al as

--
-- 
-- applyAtMu :: Viewable e => ATMu e e e -> Atom e e -> Maybe (Atom e e)
-- applyAtMu = applyAT applyAlMu
-- -- -- --
-- applyAlMu :: Viewable e =>  ALMu e -> Atom e e -> Maybe (Atom e e)
-- applyAlMu (Spn s) x = fmap I $ applyS applyAtMu (applyAl _) s (evalA x)


-- applyAlMu (Ins c d) x = undefined
--
-- applyALMuApp = applyAlMu (applyAT applyAlMu)

-- insCtx :: Ctx p -> SExpr -> Maybe b
-- insCtx (There atMu d) x = atMu
-- Test
a = Value (1)
b = Value (1)
c = Value (2)
d = Value (2)
sum1 = Add a b
sum2 = Add c d
square1 = Square a
--
--
-- data A a where
--   A1 :: Int -> A Int
--
-- data SA (a :: k -> A k) :: * where
--   SA1 :: SA 'A1
--
-- type family EA (a :: SA k) x :: k x where
--   EA SA1 x = x

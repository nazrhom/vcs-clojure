{-#LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, TypeInType, PolyKinds, AllowAmbiguousTypes, StandaloneDeriving, RankNTypes, FlexibleContexts, FlexibleInstances, TypeFamilyDependencies, ScopedTypeVariables#-}
module Diff where

import Data.Kind
import Data.Type.Equality
import Parser

-- HLIST

data HList l where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

instance Show (HList '[]) where
  show HNil = "[]"

instance (Show e, Show (HList l)) => Show (HList (e : l)) where
  show (HCons x HNil) = show x ++ "]"
  show (HCons x xs) = "[" ++ show x ++ ", " ++ show xs

type family SafeHead (l :: [*]) :: * where
  SafeHead (e ': l) = e
  SafeHead '[]      = ()

type family SafeTail (l :: [*]) :: [*] where
  SafeTail (e ': l) = l
  SafeTail '[]      = '[]

hhead :: HList (e : l) -> e
hhead (HCons e l) = e

htail :: HList (e : l) -> HList l
htail (HCons e l) = l

type e :*: l = HCons e l
infixr 2 :*:

(.*.) :: e -> HList l -> HList (e : l)
(.*.) = HCons

infixr 2 .*.
infixr 2 `HCons`

data SExpr where
  Add :: SExpr -> SExpr -> SExpr
  Square :: SExpr -> SExpr
  Value :: Val -> SExpr
  deriving (Eq, Show)

data Val = I Int | B Bool
  deriving (Eq, Show)

data SEC =
    CAdd
  | CSquare
  | CValue
  deriving (Eq, Show)

data SSEC (c :: SEC) where
  S_Add :: SSEC 'CAdd
  S_Square :: SSEC 'CSquare
  S_Value :: SSEC 'CValue

deriving instance Eq (SSEC c)
deriving instance Show (SSEC c)

instance TestEquality (SSEC) where
  testEquality S_Add S_Add = Just Refl
  testEquality S_Square S_Square = Just Refl
  testEquality S_Value S_Value = Just Refl
  testEquality _ _ = Nothing


type family TypeOf e = r | r -> e where
  TypeOf CAdd = '[SExpr, SExpr]
  TypeOf CSquare = '[SExpr]
  TypeOf CValue = '[Val]

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

type family All p i where
  All p (x:xs) = (p x x, (All p xs))
  All p '[] = ()

allHead :: All p i -> p (SafeHead i) (SafeHead i)
allHead = undefined

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

spine :: SExpr -> SExpr -> SExprSpine TrivialP TrivialL
spine x y | x == y = SScp
           | otherwise = case (view x, view y) of
             ((Tag c1 l1), (Tag c2 l2)) -> case testEquality c1 c2 of
               Just Refl -> SSCns c1 (allLTrivialP l1)
               Nothing -> SSChg c1 c2 (trivialL l1 l2)

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
data TrivialP a b where
  PP :: a -> b -> TrivialP a b
  deriving Show

data TrivialL a b where
  LL :: HList a -> HList b -> TrivialL a b

deriving instance (Show (HList a), Show (HList b)) => Show (TrivialL a b)

trivialL :: HList a -> HList b -> TrivialL a b
trivialL a b = LL a b

trivialP :: a -> b -> TrivialP a b
trivialP a b = PP a b

allTrivialP :: HList l -> All TrivialP l
allTrivialP (HCons a b) = (trivialP a a, allTrivialP b)
allTrivialP HNil = ()

allLTrivialP :: HList l -> HList (AllL TrivialP l)
allLTrivialP (HCons a b) = (trivialP a a) `HCons` (allLTrivialP b)
allLTrivialP HNil = HNil

data AI s d where
  A0 :: AI '[] '[]
  Adel :: a -> AI s d -> AI (a : s) d
  Ains :: a -> AI s d -> AI s (a : d)
  AX :: a -> b -> AI s d -> AI (a : s) (b : d)


align :: HList a -> HList b -> [AI a b]
align HNil HNil = return A0
align HNil (HCons b bs) = Ains b <$> align HNil bs
align (HCons a as) HNil = Adel a <$> align as HNil
align (HCons a as) (HCons b bs) = (AX a b <$> align as bs) ++ (Adel a <$> align as (b .*. bs)) ++ (Ains b <$> align (a .*. as) bs)


-- Test
a = Value (I 1)
b = Value (I 1)
c = Value (I 2)
d = Value (I 2)
sum1 = Add a b
sum2 = Add c d
square1 = Square a

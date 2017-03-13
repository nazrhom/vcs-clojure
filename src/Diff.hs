{-#LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, TypeInType, PolyKinds, AllowAmbiguousTypes, StandaloneDeriving, RankNTypes, FlexibleContexts, FlexibleInstances#-}
module Diff where

import Data.Kind
import Parser

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

data SEA =
    ASExpr SExpr
  | AVal Val

data SSEC (c :: SEC) where
  S_Add :: SSEC 'CAdd
  S_Square :: SSEC 'CSquare
  S_Value :: SSEC 'CValue

deriving instance Eq (SSEC c)
deriving instance Show (SSEC c)

type family TypeOf e where
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

spine :: SExpr -> SExpr -> SExprSpine Trivial Trivial
spine x y | x == y = SScp
           | otherwise = case (view x, view y) of
             ((Tag c1 l1), (Tag c2 l2)) ->
              if (consMatch c1 c2)
              then SSCns c1 (undefined)
              else SSChg c1 c2 (trivial l1 l2)

consMatch :: SSEC a -> SSEC b -> Bool
consMatch S_Add S_Add = True
consMatch S_Square S_Square = True
consMatch S_Value S_Value = True
consMatch _ _ = False

type family All p i where
  All p (x:xs) = ((p x x), (All p xs))

data SExprSpine (k :: * -> * -> *) (p :: [*] -> [*] -> *) where
  SScp :: SExprSpine k p
  SSCns :: SSEC i -> (All k (TypeOf i)) -> SExprSpine k p
  SSChg :: SSEC i -> SSEC j -> p (TypeOf i) (TypeOf j) -> SExprSpine k p

data HList l where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

instance Show (HList '[]) where
  show HNil = "[]"

instance (Show e, Show (HList l)) => Show (HList (e : l)) where
  show (HCons x HNil) = show x ++ "]"
  show (HCons x xs) = "[" ++ show x ++ ", " ++ show xs

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

data Trivial l r where
  P1 :: a -> b -> Trivial l r

data AI s d where
  A0 :: AI '[] '[]
  Adel :: a -> AI s d -> AI (a : s) d
  Ains :: a -> AI s d -> AI s (a : d)
  AX :: a -> b -> AI s d -> AI (a : s) (b : d)


sExprToSEC :: SExpr -> SEC
sExprToSEC (Add a b) = CAdd
sExprToSEC (Square a) = CSquare
sExprToSEC (Value a) = CValue

-- asHList (Add a b) = a .*. b .*. HNil
-- asHList (Square a) = a .*. HNil
-- asHList (Value a) = a .*. HNil
-- sExprToSSEC :: SExpr -> SSEC SEC
-- sExprToSSEC = undefined

trivial :: HList a -> HList b -> Trivial a b
trivial a b = P1 a b

align :: HList a -> HList b -> [AI a b]
align HNil HNil = return A0
align HNil (HCons b bs) = Ains b <$> align HNil bs
align (HCons a as) HNil = Adel a <$> align as HNil
align (HCons a as) (HCons b bs) = (AX a b <$> align as bs) ++ (Adel a <$> align as (b .*. bs)) ++ (Ains b <$> align (a .*. as) bs)

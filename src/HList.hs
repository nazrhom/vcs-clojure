{-#LANGUAGE GADTs, TypeOperators, KindSignatures, TypeFamilies, TypeInType, FlexibleInstances, FlexibleContexts#-}
module HList where

import Data.Kind

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

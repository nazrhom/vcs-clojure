{-#LANGUAGE GADTs, KindSignatures, StandaloneDeriving, TypeFamilyDependencies, DataKinds#-}

module Lang where

import Data.Type.Equality

data SExpr where
  Add :: SExpr -> SExpr -> SExpr
  Square :: SExpr -> SExpr
  Value :: Int -> SExpr
  deriving (Eq, Show)

data Val = IVal Int | BVal Bool
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


instance TestEquality SSEC where
  testEquality S_Add S_Add = Just Refl
  testEquality S_Square S_Square = Just Refl
  testEquality S_Value S_Value = Just Refl
  testEquality _ _ = Nothing

type family TypeOf e where
  TypeOf CAdd = '[SExpr, SExpr]
  TypeOf CSquare = '[SExpr]
  TypeOf CValue = '[Int]

type family UniverseOf a where
  UniverseOf CAdd = SExpr
  UniverseOf CSquare = SExpr
  UniverseOf CValue = SExpr

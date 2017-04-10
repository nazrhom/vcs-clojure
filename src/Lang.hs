{-#LANGUAGE GADTs, KindSignatures, StandaloneDeriving, TypeFamilyDependencies, DataKinds#-}

module Lang where

import Data.Type.Equality

data SExpr where
  Add :: SExpr -> SExpr -> SExpr
  Square :: SExpr -> SExpr
  Value :: Int -> SExpr
  deriving (Eq, Show)

data Constr =
    CAdd
  | CSquare
  | CValue
  deriving (Eq, Show)

data SExprConstr :: Constr -> * where
  S_Add :: SExprConstr 'CAdd
  S_Square :: SExprConstr 'CSquare
  S_Value :: SExprConstr 'CValue

deriving instance Eq (SExprConstr c)
deriving instance Show (SExprConstr c)

instance TestEquality SExprConstr where
  testEquality S_Add S_Add = Just Refl
  testEquality S_Square S_Square = Just Refl
  testEquality S_Value S_Value = Just Refl
  testEquality _ _ = Nothing

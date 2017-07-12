{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.NoDupBranches where

import Control.Monad.Reader

import Oracle.Internal
import Clojure.Lang

data NoDupBranches = NoDupBranches

nextPaths :: [Path] -> [Path]
nextPaths []        = [I , M , D]
nextPaths (I:_)     = [I , M]
nextPaths (D:_)     = [D , M]
nextPaths (M:_)     = [I , M , D]

instance (Monad m) => OracleP NoDupBranches m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return []
  callP _ (_ `Ac` _) An         = return [D]
  callP _ (_ `Ac` _) (_ `Ac` _) = ask >>= return . nextPaths

instance (Monad m) => OracleF NoDupBranches m where
  callF _ _ _  = ask >>= return . nextPaths

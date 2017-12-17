{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.NoDupBranches where

import Control.Monad.Reader
import Data.Maybe
import Debug.Trace

import Oracle.Internal
import Language.Common

data NoDupBranches = NoDupBranches

nextPaths :: [Path] -> [Path]
nextPaths (I:_)     = [I, M]
nextPaths (D:_)     = [D, M]
nextPaths (M:_)     = [I, M, D]

instance (Monad m) => OracleP NoDupBranches m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [I]
  callP _ (_ `Ac` _) An         = return [D]
  callP _ (s `Ac` _) (d `Ac` _) = ask >>= return . nextPaths

instance (Monad m) => OracleF NoDupBranches m where
  callF _ s d = ask >>= return . nextPaths
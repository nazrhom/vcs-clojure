{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Oracle.DisjointOracle where

import Language.Common
import VCS.Multirec
import Oracle.Internal
import Control.Monad.State

data DisjointOracle u = forall v . DisjointOracle (Almu u v)

-- flatten :: Almu u v -> Usingl u -> ?
-- flatten (Alspn s) u =

instance (Monad m) => OracleP (DisjointOracle u) (StateT (Almu u v) m) where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [I]
  callP _ (_ `Ac` _) An         = return [D]
  callP (DisjointOracle almu) (a1 `Ac` as1) (a2 `Ac` as2)
    = undefined
    -- do
    -- case almu of
    --   Alspn s -> _
    --   Alins c ctx -> _
    --   Aldel c ctx -> _

instance (Monad m) => OracleF (DisjointOracle u) m where
  callF (DisjointOracle almu) u v = do
    case almu of
      Alspn s -> return [M, D, I]
      Alins c ctx -> return [D, M]
      Aldel c ctx -> return [I, M]

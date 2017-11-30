{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.NoOracle where

import Oracle.Internal
import Language.Common


-- * The "dumb" oracle, then, is:
data NoOracle = NoOracle
instance (Monad m) => OracleP NoOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [I]
  callP _ (_ `Ac` _) An         = return [D]
  callP _ _          _          = return [I , M , D]

instance (Monad m) => OracleF NoOracle m where
  callF _ _ _ = return [I , M , D]

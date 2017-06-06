{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Oracle where

import Control.Applicative
import Data.Type.Equality hiding (apply)

import Lang

data Phase
  = I | M | D
  deriving (Eq , Show)
--
-- Oracle m o a where
--   call (Ann f a) => [options] -> o -> f a -> f a -> m [options]

-- the f is free as it represents different ast data types which are all annotated by a
--  The first options list is the sequence of choices made up to that point, the return list is the list of all next steps that should be taken
-- options = Phase
class Ann f a where
  extract :: f a -> a
-- instance Ann (All Usingl p1) () where
--   extract = const ()

class (Monad m) => Oracle m o a where
  call :: (Ann f a , Ann g a) => o -> f a -> g a -> m (o, [Phase])
--
-- instance (Monad m) => Oracle m () () where
--   call :: (Ann (All Usingl p1) (), Ann (All Usingl p2) ()) =>
--           () -> All Usingl p1 -> All Usingl p2 ->
--           m ((), [Phase])
--   call _ An _ = return ((), [I])
--   call _ _ An = return ((), [D])
--   call _ _ _  = return ((), [I,M,D])

type NoOptOracle = ()
type Opt1Oracle = [Phase]

-- instance (Monad m) => Oracle m [Phase] where
--   call (I:)

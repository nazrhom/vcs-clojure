{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.NoIllegalCopy where

import Oracle.Internal

import Clojure.Lang
import Clojure.AST
import Clojure.Parser

data NoIllegalCopy = NoIllegalCopy

-- instance (Monad m) => OracleF NoIllegalCopy m where
--   callF o s d = return $ askOracle s d
--
-- instance (Monad m) => OracleP NoIllegalCopy m where
--   callP _ An         An         = return []
--   callP _ An         (_ `Ac` _) = return [ I ]
--   callP _ (_ `Ac` _) An         = return [ D ]
--   callP o (s `Ac` _) (d `Ac` _) = return $ askOracle s d
--
-- -- askOracle :: Usingl u -> Usingl v -> [Path]
-- -- askOracle (USepExprList (Cons e1 _ (Nil _) _)) (USepExprList (Cons e3 _ e4 _)) = [ I, M ]
-- -- askOracle (USepExprList (Cons e1 _ e2 _)) (USepExprList (Cons e3 _ (Nil _) _)) = [ D, M ]
-- -- askOracle (UExpr (Seq e1 (Empty _) _)) (UExpr (Seq e2 e3 _)) = [ I, M ]
-- -- askOracle (UExpr (Seq e1 e2 _)) (UExpr (Seq e3 (Empty _) _)) = [ D, M ]
-- -- askOracle _ _ = [ ]

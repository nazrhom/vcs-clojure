{-# LANGUAGE GADTs #-}

module Language.Presentation.Cost where

import Language.Presentation.AST
import Language.Presentation.Lang

costUsingl :: Usingl u -> Int
costUsingl (UInt u) = 1
costUsingl (UT t) = costT t
costUsingl (UA a) = costA a

costT :: T -> Int
costT (Sum t1 t2)  = costT t1 + costT t2
costT (Atom a)     = costA a

costA :: A -> Int
costA (Parens t) = costT t
costA (Val v)    = 1
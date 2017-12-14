{-# LANGUAGE GADTs #-}

module Language.BinaryTree.Cost where

import Language.BinaryTree.AST
import Language.BinaryTree.Lang

costUsingl :: Usingl u -> Int
costUsingl (UInt u) = 1
costUsingl (UIntTree t) = costIntTree t

costIntTree :: IntTree -> Int
costIntTree (Node t1 t2) = costIntTree t1 + costIntTree t2
costIntTree (Leaf i)     = 1

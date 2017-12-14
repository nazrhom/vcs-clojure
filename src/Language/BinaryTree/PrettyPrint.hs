{-# LANGUAGE GADTs #-}

module Language.BinaryTree.PrettyPrint where

import Text.PrettyPrint.Leijen
import Language.BinaryTree.Lang
import Language.BinaryTree.AST

ppConstr :: ConstrFor u c -> Doc
ppConstr NodeProof = text "Node"
ppConstr LeafProof = text "Leaf"

ppUsingl :: Usingl u -> Doc
ppUsingl (UInt i)     = text (show i)
ppUsingl (UIntTree t) = ppIntTree t

ppIntTree :: IntTree -> Doc
ppIntTree (Node t1 t2) = ppIntTree t1 <> ppIntTree t2
ppIntTree (Leaf i)     = text (show i)
module Language.BinaryTree.AST where

data IntTree = Node IntTree IntTree | Leaf Int
  deriving (Show,Eq)

t1 :: IntTree
t1 = Node (Leaf 1) (Node (Leaf 1) (Leaf 1))

t2 :: IntTree
t2 = Node (Leaf 1) (Leaf 2)
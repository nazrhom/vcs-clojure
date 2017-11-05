module Util.Treeview where

import Clojure.AST
import Control.Monad.State

type CurrentLine = Int
type LineMonad = State CurrentLine

toTree :: Expr -> String
toTree e = evalState (toTreeExpr e) 1

newLinesToStartOfRange :: CurrentLine -> LineRange -> Int
newLinesToStartOfRange cl (Range s e) = s - cl

insertNewLines :: Int -> String
insertNewLines i = replicate i '\n'

updateLineCounter :: LineRange -> LineMonad Int
updateLineCounter lr = do
  cl <- get
  let advance = newLinesToStartOfRange cl lr
  put (cl + advance)
  return advance

toTreeExpr :: Expr -> LineMonad String
toTreeExpr (Special fty e lr) = do
  curr <- updateLineCounter lr
  e1 <- toTreeExpr e
  return $ insertNewLines curr ++ "Special " ++ show fty ++ " " ++ e1
toTreeExpr (Dispatch e lr) = do
  curr <- updateLineCounter lr
  e1 <- toTreeExpr e
  return $ insertNewLines curr ++ "Dispatch " ++ e1
toTreeExpr (Collection cty sel lr) = do
  curr <- updateLineCounter lr
  sel1 <- toTreeSel sel
  return $ insertNewLines curr ++ "Collection " ++ show cty ++ " " ++ sel1
toTreeExpr (Term t lr) = do
  curr <- updateLineCounter lr
  t1 <- toTreeTerm t
  return $ insertNewLines curr ++ "Term " ++ t1
toTreeExpr (Comment s lr) = do
  curr <- updateLineCounter lr
  return $ insertNewLines curr ++ "Comment " ++ show s
toTreeExpr (Seq e1 e2 lr) = do
  curr <- updateLineCounter lr
  e1S <- toTreeExpr e1
  e2S <- toTreeExpr e2
  return $ insertNewLines curr ++ "Seq " ++ e1S ++ " " ++ e2S
toTreeExpr (Empty lr) = do
  curr <- updateLineCounter lr
  return $ insertNewLines curr ++ "Empty"

toTreeSel :: SepExprList -> LineMonad String
toTreeSel (Nil lr) = do
  curr <- updateLineCounter lr
  return $ insertNewLines curr ++ "Nil"
toTreeSel (Cons e sep sel lr) = do
  curr <- updateLineCounter lr
  eS <- toTreeExpr e
  selS <- toTreeSel sel
  return $ insertNewLines curr ++ "Cons " ++ eS ++ " " ++ show sep ++ " " ++ selS

toTreeTerm :: Term -> LineMonad String
toTreeTerm (TaggedString tag str lr) = do
  curr <- updateLineCounter lr
  return $ insertNewLines curr ++ "TaggedString " ++ show tag ++ " " ++ show str
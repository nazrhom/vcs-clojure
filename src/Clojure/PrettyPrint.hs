{-# LANGUAGE GADTs #-}

module Clojure.PrettyPrint
    ( ppTop
    , ppLines
    , ppExpr
    , ppTerm
    , ppSepExprList
    , ppUsingl
    , ppConstr
    ) where

import Text.PrettyPrint.Leijen
import Clojure.Lang
import Clojure.AST

ppTop :: Expr -> String
ppTop es = show $ ppExpr es

ppLines :: [Expr] -> String
ppLines es = show $ vvcat $ map ppExpr es
  where
    vvcat ds = vcat $ punctuate line ds

ppExpr :: Expr -> Doc
ppExpr (Special fty e _) = ppFormTy fty <> ppExpr e
ppExpr (Dispatch e _) = char '#' <> ppExpr e
ppExpr (Collection cty es _) = ppCollType cty $ ppSepExprList es
ppExpr (Comment s _) = char ';' <> text s <> linebreak
ppExpr (Term t _) = ppTerm t
ppExpr (Seq p1 p2 _) = ppExpr p1 <$$> ppExpr p2
ppExpr (Empty _) = empty

ppCollType :: String -> (Doc -> Doc)
ppCollType s = case s of
  "Parens" -> parens
  "Vec" -> brackets
  "Set" -> braces
  _     -> const $ error "nosense collty"

ppFormTy :: String -> Doc
ppFormTy s = case s of
  "Quote" ->  char '\''
  "SQuote" ->  char '`'
  "UnQuote" ->  char '~'
  "DeRef" ->  char '@'
  _ -> error "nosense formty"

ppTerm :: Term -> Doc
ppTerm (TaggedString tag s _) = case tag of
  "String" -> dquotes $ text s
  "Metadata" -> char '^' <> text s
  "Var" -> text s
  _     -> error "nosense term"

ppSepExprList :: SepExprList -> Doc
ppSepExprList (Nil _)         = empty
ppSepExprList (Cons x sep xs _) = ppExpr x <> ppSep sep <> ppSepExprList xs

ppSep :: String -> Doc
ppSep s = case s of
  "Comma" -> char ','
  "NewLine" -> linebreak
  "Space" -> space
  "Empty" -> empty
  _    -> error "nosense sep"

ppSepPair :: [Expr] -> Doc -> Doc
ppSepPair xs sep = hsep $ punctuate sep (go xs)
 where
   go :: [Expr] -> [Doc]
   go (k:v:rest) = (ppExpr k <+> ppExpr v):(go rest)
   go [] = []

ppUsingl :: Usingl u -> Doc
ppUsingl (UString u) = text u
ppUsingl (USepExprList u) = ppSepExprList u
ppUsingl (UExpr u) = ppExpr u
ppUsingl (UTerm u) = ppTerm u

ppConstr :: ConstrFor u c -> Doc
ppConstr C1NilProof = text "Nil"
ppConstr C1ConsProof = text "Cons"
ppConstr C3SpecialProof = text "Special"
ppConstr C3DispatchProof = text "Dispatch"
ppConstr C3CollectionProof = text "Collection"
ppConstr C3TermProof = text "Term"
ppConstr C3CommentProof = text "Comment"
ppConstr C3SeqProof = text "Seq"
ppConstr C3EmptyProof = text "Empty"
ppConstr C6TaggedStringProof = text "TaggedString"
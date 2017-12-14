{-# LANGUAGE GADTs #-}

module Language.Clojure.PrettyPrint
    ( ppTop
    , ppLines
    , ppExpr
    , ppTerm
    , ppSepExprList
    , ppUsingl
    , ppConstr
    ) where

import Text.PrettyPrint.Leijen
import Language.Clojure.Lang
import Language.Clojure.AST
import Data.Text as T hiding (empty, map)

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
ppExpr (Comment s _) = char ';' <> text (T.unpack s) <> linebreak
ppExpr (Term t _) = ppTerm t
ppExpr (Seq p1 p2 _) = ppExpr p1 <$$> ppExpr p2
ppExpr (Empty _) = empty

ppCollType :: CollTy -> (Doc -> Doc)
ppCollType Parens = parens
ppCollType Vec = brackets
ppCollType Set = braces

ppFormTy :: FormTy -> Doc
ppFormTy Quote   = char '\''
ppFormTy SQuote  = char '`'
ppFormTy UnQuote = char '~'
ppFormTy DeRef   = char '@'

ppTerm :: Term -> Doc
ppTerm (TaggedString String s _) = dquotes $ text (T.unpack s)
ppTerm (TaggedString Metadata s _) = char '^' <> text (T.unpack s)
ppTerm (TaggedString Var s _) = text (T.unpack s)

ppSepExprList :: SepExprList -> Doc
ppSepExprList (Nil _)         = empty
ppSepExprList (Cons x sep xs _) = ppExpr x <> ppSep sep <> ppSepExprList xs

ppSep :: Sep -> Doc
ppSep Comma = char ','
ppSep NewLine = linebreak
ppSep Space = space
ppSep EmptySep = empty

ppSepPair :: [Expr] -> Doc -> Doc
ppSepPair xs sep = hsep $ punctuate sep (go xs)
 where
   go :: [Expr] -> [Doc]
   go (k:v:rest) = (ppExpr k <+> ppExpr v):(go rest)
   go [] = []

ppUsingl :: Usingl u -> Doc
ppUsingl (UString u) = text (T.unpack u)
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
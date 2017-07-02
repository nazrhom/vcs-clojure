module Clojure.PrettyPrint
    ( ppTop
    , ppLines
    ) where

import Text.PrettyPrint.Leijen
import Clojure.AST

ppTop :: Expr -> String
ppTop es = show $ ppExpr es

ppLines :: [Expr] -> String
ppLines es = show $ vvcat $ map ppExpr es
  where
    vvcat ds = vcat $ punctuate line ds

ppExpr :: Expr -> Doc
ppExpr (Special Quote e _) = char '\'' <> ppExpr e
ppExpr (Special SQuote e _) = char '`' <> ppExpr e
ppExpr (Special UnQuote e _) = char '~' <> ppExpr e
ppExpr (Special DeRef e _) = char '@' <> ppExpr e
ppExpr (Dispatch e _) = char '#' <> ppExpr e
ppExpr (Collection Parens es _) = parens $ ppSepExprList es
ppExpr (Collection Vec es _) = brackets $ ppSepExprList es
ppExpr (Collection Set es _) = braces $ ppSepExprList es
ppExpr (Comment s _) = char ';' <> text s <> linebreak
ppExpr (Term t _) = ppTerm t
ppExpr (Seq p q) = ppExpr p <> line <> ppExpr q

ppTerm :: Term -> Doc
ppTerm (TaggedString String s) = dquotes $ text s
ppTerm (TaggedString Metadata s) = char '^' <> text s
ppTerm (TaggedString Var s) = text s

ppSepExprList :: SepExprList -> Doc
ppSepExprList Nil         = empty
ppSepExprList (Singleton a) = ppExpr a
ppSepExprList (Cons x sep xs) = ppExpr x <> ppSep sep <> ppSepExprList xs

ppSep :: Sep -> Doc
ppSep Comma = char ','
ppSep Space = space
ppSep NewLine = linebreak

ppSepPair :: [Expr] -> Doc -> Doc
ppSepPair xs sep = hsep $ punctuate sep (go xs)
 where
   go :: [Expr] -> [Doc]
   go (k:v:rest) = (ppExpr k <+> ppExpr v):(go rest)
   go [] = []

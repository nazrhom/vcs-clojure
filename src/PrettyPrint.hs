module PrettyPrint
    ( ppTop
    ) where

import Text.PrettyPrint.Leijen
import Parser

ppTop :: [Expr] -> String
ppTop es = show $ vvcat $ map ppExpr es
  where
    vvcat ds = vcat $ punctuate line ds

ppExpr :: Expr -> Doc
ppExpr (Special Quote e) = char '\'' <> ppExpr e
ppExpr (Special SQuote e) = char '`' <> ppExpr e
ppExpr (Special UnQuote e) = char '~' <> ppExpr e
ppExpr (Special SUnQuote e) = text "~@" <> ppExpr e
ppExpr (Special DeRef e) = char '@' <> ppExpr e
ppExpr (Dispatch e) = char '#' <> ppExpr e
ppExpr (Collection Parens es) = parens $ ppSepList es
ppExpr (Collection Vec es) = brackets $ ppSepList es
ppExpr (Collection Set es) = braces $ ppSepList es
ppExpr (Comment s) = char ';' <> text s <> linebreak
ppExpr (Term t) = ppTerm t

ppTerm :: Term -> Doc
ppTerm (TaggedString String s) = dquotes $ text s
ppTerm (TaggedString Metadata s) = char '^' <> text s
ppTerm (TaggedString Var s) = text s 

ppSepList :: SepList Expr -> Doc
ppSepList Nil         = empty
ppSepList (Singleton a) = ppExpr a
ppSepList (Cons x sep xs) = ppExpr x <> ppSep sep <> ppSepList xs

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

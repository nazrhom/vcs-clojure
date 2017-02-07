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
ppExpr (Parens es) = parens $ hsep (map ppExpr es)
ppExpr (Special Quote e) = char '\'' <> ppExpr e
ppExpr (Special SQuote e) = char '`' <> ppExpr e
ppExpr (Special UnQuote e) = char '~' <> ppExpr e
ppExpr (Special SUnQuote e) = text "~@" <> ppExpr e
ppExpr (Dispatch e) = char '#' <> ppExpr e
ppExpr (Collection Vec es) = brackets $ hsep (map ppExpr es)
ppExpr (Collection Bindings es) = brackets $ ppCommaSepPair es
ppExpr (Collection Map es) = braces $ ppCommaSepPair es
ppExpr (Collection Set es) = braces $ hsep (map ppExpr es)
ppExpr (Comment s) = char ';' <> text s <> linebreak
ppExpr (Term t) = ppTerm t

ppTerm :: Term -> Doc
ppTerm (TaggedString String s) = dquotes $ text s
ppTerm (TaggedString Metadata s) = text "metadata"
ppTerm (TaggedString Var s) = text s -- can be refined by inspecting (maybe parsing) s
ppTerm (Int i) = text $ show i
ppTerm Nil = text "nil"

ppCommaSepPair :: [Expr] -> Doc
ppCommaSepPair xs = hsep $ punctuate comma (go xs)
 where
   go :: [Expr] -> [Doc]
   go (k:v:rest) = (ppExpr k <+> ppExpr v):(go rest)
   go [] = []

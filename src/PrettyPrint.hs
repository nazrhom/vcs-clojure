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
ppExpr (Special fty e) = ppFormTy fty <> ppExpr e
ppExpr (Dispatch e) = char '#' <> ppExpr e
ppExpr (Collection cty es) = ppCollType cty $ ppSepExprList es
ppExpr (Comment s) = char ';' <> text s <> linebreak
ppExpr (Term t) = ppTerm t


ppCollType :: String -> (Doc -> Doc)
ppCollType s = case s of
  "Parens" -> parens
  "Vec" -> brackets
  "Set" -> braces
  _     -> id
ppFormTy :: String -> Doc
ppFormTy s = case s of
  "Quote" ->  char '\''
  "SQuote" ->  char '`'
  "UnQuote" ->  char '~'
  "SUnQuote" ->  text "~@"
  "DeRef" ->  char '@'
  _ -> empty

ppTerm :: Term -> Doc
ppTerm (TaggedString tag s) = case tag of
  "String" -> dquotes $ text s
  "Metadata" -> char '^' <> text s
  "Var" -> text s

ppSepExprList :: SepExprList -> Doc
ppSepExprList Nil         = empty
ppSepExprList (Singleton a) = ppExpr a
ppSepExprList (Cons x sep xs) = ppExpr x <> ppSep sep <> ppSepExprList xs

ppSep :: String -> Doc
ppSep s = case s of
  "Comma" -> char ','
  "NewLine" -> space
  "Space" ->linebreak
  _    -> empty

ppSepPair :: [Expr] -> Doc -> Doc
ppSepPair xs sep = hsep $ punctuate sep (go xs)
 where
   go :: [Expr] -> [Doc]
   go (k:v:rest) = (ppExpr k <+> ppExpr v):(go rest)
   go [] = []

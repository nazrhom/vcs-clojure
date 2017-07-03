module Clojure.Parser
    ( parseTop
    , parse
    , parseTest

    -- AST
    , Expr(..)
    , FormTy(..)
    , CollType(..)
    , Term(..)
    , Tag(..)
    , SepExprList(..)
    , Sep(..)
    ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char hiding (Space)

import Clojure.AST

lexer = makeTokenParser javaStyle
  { identStart = alphaNum <|> oneOf "_':*-&\\,"
  , identLetter = alphaNum <|> oneOf ":_.,'-/^?!><*#\"\\" <|> satisfy isSymbol
  }

parseSeq :: Parsec String () Expr
parseSeq = do
  start <- getPosition
  p1 <- parseExpr
  p2 <- (try parseSeq <|> parseExpr)
  end <- getPosition
  return $ Seq p1 p2 (mkRange start end)

parseTop = whiteSpace lexer *> (try parseSeq <|> parseExpr) <* eof

parseExpr = lexeme lexer $ choice
  [ parseSpecial
  , parseDispatch
  , parseCollection
  , parseComment
  , parseTerm
  ]

parseTerm =
  do
    start <- getPosition
    term <- parseTaggedString
    end <- getPosition
    return $ Term term (mkRange start end)

parseCollection = choice [ parseParens, parseVec, parseSet ]

parseSpecial = do
  start <- getPosition
  ident <- parseSpecialIdent
  expr <- parseExpr
  end <- getPosition
  return $ Special ident expr (mkRange start end)

parseSpecialIdent = choice
  [ "Quote" <$ char '\''
  , "SQuote" <$ char '`'
  , "UnQuote" <$ char '~'
  , "DeRef" <$ char '@'
  ]

parseTaggedString = choice [parseString, parseVar, parseMetadata]

parseDispatch = do
  start <- getPosition
  char '#'
  disp <- parseDispatchable
  end <- getPosition
  return $ Dispatch disp (mkRange start end)
  where
    parseDispatchable = choice
      [ parseSet
      , parseRegExp
      , parseParens
      , parseTaggedLit
      , parseMeta
      ]
    --- ref: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/
    -- parseParens covers the function marco
    -- parseTaggedLit covers the var macro (as identifiers can start with a quote ('))
    parseRegExp = do
      start <- getPosition
      regExp <- parseString
      end <- getPosition
      return $ Term regExp (mkRange start end)

    parseTaggedLit = do
      start <- getPosition
      tLit <- parseVar
      end <- getPosition
      return $ Term tLit (mkRange start end)

    parseMeta = do
      start <- getPosition
      meta <- parseMetadata
      end <- getPosition
      return $ Term meta (mkRange start end)

parseComment = do
  start <- getPosition
  char ';'
  comment <- manyTill anyChar (string "\n")
  end <- getPosition
  return $ Comment comment (mkRange start end)

parseParens = do
  start <- getPosition
  contents <- (parens lexer) parseSepExprList
  end <- getPosition
  return $ Collection "Parens" contents (mkRange start end)
--
parseSet = do
  start <- getPosition
  contents <- (braces lexer) parseSepExprList
  end <- getPosition
  return $ Collection "Set" contents (mkRange start end)

parseVec = do
  start <- getPosition
  contents <- (brackets lexer) parseSepExprList
  end <- getPosition
  return $ Collection "Vec" contents (mkRange start end)

parseSepExprList = try parseSepExprList1 <|> parseSingleton <|> parseEmptyList

parseEmptyList = do
  start <- getPosition
  return $ Nil (mkRange start start)

parseSingleton = do
  start <- getPosition
  expr <- parseExpr
  end <- getPosition
  return $ Singleton expr (mkRange start end)
--
parseSepExprList1 = do
  start <- getPosition
  x <- parseExpr
  sep <- parseSep
  xs <- parseSepExprList
  end <- getPosition
  return $ Cons x sep xs (mkRange start end)

parseSep = choice
  [ "Space" <$ whiteSpace lexer
  , "Comma" <$ lexeme lexer (char ',')
  , "NewLine" <$ lexeme lexer (newline)
  ]

parseString = do
  start <- getPosition
  qstring <- quotedString
  end <- getPosition
  return $ TaggedString "String" qstring (mkRange start end)
  where
    quotedString :: Parsec String () String
    quotedString = do
      char '"'
      x <- many (try (try (string "\\\\") <|> string "\\\"") <|> fmap pure (noneOf "\""))
      char '"'
      return $ concat x

parseVar = do
  start <- getPosition
  vstring <- (identifier lexer <|> operator lexer)
  end <- getPosition
  return $ TaggedString "Var" vstring (mkRange start end)

parseMetadata = do
  start <- getPosition
  char '^'
  meta <- identifier lexer
  end <- getPosition
  return $ TaggedString "Metadata" meta (mkRange start end)

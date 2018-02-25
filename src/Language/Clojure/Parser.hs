{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Language.Clojure.Parser
    ( parseTop
    , parse
    , parseTest
    , parseAsExprList

    -- AST
    , Expr(..)
    , FormTy(..)
    , CollTy(..)
    , Term(..)
    , Tag(..)
    , SepExprList(..)
    , Sep(..)
    , LineRange(..)
    ) where

import Text.Parsec hiding (Empty)
import Text.Parsec.Token hiding (braces, parens, brackets, identifier, operator)
import Text.Parsec.Language
import Data.Char hiding (Space)
import qualified Data.Text as T
import Data.Proxy

import Language.Clojure.AST

lexer = makeTokenParser javaStyle
  { identStart = alphaNum <|> oneOf "_':*-&."
  , identLetter = alphaNum <|> oneOf ":_.'-/^?!><*#\"\\" <|> satisfy isSymbol
  }

parseSeq :: Parsec String () Expr
parseSeq = do
  start <- getPosition
  p1 <- parseExpr
  whiteSpace lexer
  p2 <- (try parseSeq <|> parseEmptyProgram)
  end <- getPosition
  return $ Seq p1 p2 (mkRange start end)

parseTop = whiteSpace lexer *> (try parseSeq <|> parseEmptyProgram) <* eof

parseAsExprList = do
  top <- parseTop
  return $ walkSeq top
  -- whiteSpace lexer *> (many parseSingleExpr) <* whiteSpace lexer <* eof

walkSeq (Seq a (Empty _) _) = a : []
walkSeq (Seq a b _) = a : walkSeq b
walkSeq (Empty lr) = [Empty lr]
walkSeq e = error $ "nowalk" ++ show e

parseExpr = choice
  [ try parseSpecial
  , try parseDispatch
  , parseCollection
  , parseComment
  , parseTerm
  ]

parseEmptyProgram = do
  pos <- getPosition
  return $ Empty (mkRange pos pos)

parseTerm = do
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
  [ Quote <$ char '\''
  , SQuote <$ char '`'
  , UnQuote <$ char '~'
  , DeRef <$ char '@'
  , Meta <$ char '^'
  ]

parseTaggedString = choice [parseString, parseVar]

parseDispatch = do
  start <- getPosition
  char '#'
  disp <- parseDispatchable
  end <- getPosition
  return $ Dispatch disp (mkRange start end)
  where
    parseDispatchable = choice
      [ parseExpr
      , parseRegExp
      , parseTaggedLit
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

    -- parseMeta = do
    --   start <- getPosition
    --   meta <- parseMetadata
    --   end <- getPosition
    --   return $ Term meta (mkRange start end)

parseComment = do
  start <- getPosition
  char ';'
  comment <- manyTill anyChar (newline <|> eofS)
  -- single line comment, if we parse end here we have parsed newline as well
  return $ Comment (T.pack comment) (mkRange start start)

eofS = do
  eof
  return '\n'

parens p = between (symbol lexer "(") (string ")") p
braces p = between (symbol lexer "{") (string "}") p
brackets p = between (symbol lexer "[") (string "]") p


parseSet = do
  start <- getPosition
  contents <- braces parseSepExprList
  end <- getPosition
  return $ Collection Set contents (mkRange start end)

parseVec = do
  start <- getPosition
  contents <- brackets parseSepExprList
  end <- getPosition
  return $ Collection Vec contents (mkRange start end)

parseParens = do
  start <- getPosition
  contents <- parens parseSepExprList
  end <- getPosition
  return $ Collection Parens contents (mkRange start end)

parseSepExprList = parseSepExprList1 <|> parseEmptyList

parseEmptyList = do
  start <- getPosition
  return $ Nil (mkRange start start)

parseSepExprList1 = do
  start <- getPosition
  x <- parseExpr
  sep <- parseSep
  xs <- parseSepExprList
  end <- getPosition
  return $ Cons x sep xs (mkRange start end)

parseSep = choice
  [ Comma <$ lexeme lexer (char ',')
  , NewLine <$ lexeme lexer (many1 endOfLine)
  , Space <$ lexeme lexer (many1 (space <|> tab))
  , EmptySep <$ (lookAhead (anyChar) <|> eofS)
  ]
parseString = do
  start <- getPosition
  qstring <- quotedString
  end <- getPosition
  return $ TaggedString String (T.pack qstring) (mkRange start end)
  where
    quotedString :: Parsec String () String
    quotedString = do
      char '"'
      x <- many (try (try (string "\\\\") <|> string "\\\"") <|> fmap pure (noneOf "\""))
      char '"'
      return $ concat x

parseVar = do
  start <- getPosition
  vstring <- (identifier)
  end <- getPosition
  return $ TaggedString Var (T.pack vstring) (mkRange start end)

identifier = do
  c <- alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~_',"
  cs <- many (alphaNum <|> oneOf ":!?#$%&*+-/.<=>'?@^|~_'^\"\\" <|> satisfy isSymbol)
  return (c:cs)

-- parseMetadata = do
--   start <- getPosition
--   char '^'
--   meta <- identifier
--   end <- getPosition
--   return $ TaggedString Metadata (T.pack meta) (mkRange start end)

module Parser
    ( parseTop
    , parseManyExpr
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

data SepExprList = Nil | Singleton Expr | Cons Expr Sep SepExprList deriving (Show, Eq)

data Sep = Space | Comma | NewLine deriving (Show, Eq)

data Expr = Special FormTy Expr SourcePos
          | Dispatch Expr SourcePos
          | Collection CollType (SepExprList) SourcePos
          | Term Term SourcePos
          | Comment String SourcePos
          | Seq Expr Expr
          deriving (Show, Eq)

-- ref: https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html
data FormTy = Quote | SQuote | UnQuote | SUnQuote | DeRef deriving (Show, Eq)

data CollType = Vec | Set | Parens deriving (Show, Eq)

data Term = TaggedString Tag String
          deriving (Show, Eq)

data Tag = String | Metadata | Var  deriving (Show, Eq)

lexer = makeTokenParser javaStyle
  { identStart = alphaNum <|> oneOf "_':*-&\\,"
  , identLetter = alphaNum <|> oneOf ":_.,'-/^?!><*#\"\\" <|> satisfy isSymbol
  }

parseTop :: Parsec String () Expr
parseTop = do
  p <- parseManyExpr
  return $ foldl1 Seq p

parseManyExpr = whiteSpace lexer *> many parseExpr <* eof

parseExpr = lexeme lexer $ choice
  [ parseSpecial
  , parseDispatch
  , parseCollection
  , parseComment
  , Term <$> parseTerm <*> getPosition
  ]

parseTerm = lexeme lexer $ parseTaggedString

parseCollection = choice [ parseParens, parseVec, parseSet ]

parseSpecial = choice [parseQuote, parseSQuote, try parseSUnQuote, parseUnQuote, parseDeRef]

parseQuote = Special <$> pure Quote <* char '\'' <*> parseExpr <*> getPosition

parseSQuote = Special <$> pure SQuote <* char '`' <*> parseExpr <*> getPosition

parseSUnQuote = Special <$> pure SUnQuote <* char '~' <* char '@' <*> parseExpr <*> getPosition

parseUnQuote = Special <$> pure UnQuote <* char '~' <* optional (char '@') <*> parseExpr <*> getPosition

parseDeRef = Special <$> pure DeRef <* char '@' <*> parseExpr <*> getPosition

parseTaggedString = choice [parseString, parseVar, parseMetadata]


parseDispatch = Dispatch <$ char '#' <*> parseDispatchable <*> getPosition
  where
    parseDispatchable = parseSet <|> parseDiscard <|> parseRegExp <|> parseParens <|> parseTaggedLit <|> parseMeta
    --- ref: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/
    -- parseParens covers the function marco
    -- parseTaggedLit covers the var macro (as identifiers can start with a quote ('))
    parseDiscard = Term <$> (TaggedString <$> pure Var <*> string "_") <*> getPosition
    parseRegExp = Term <$> parseString <*> getPosition
    parseTaggedLit = Term <$> parseVar <*> getPosition
    parseMeta = Term <$> parseMetadata <*> getPosition

parseComment = Comment <$ char ';' <*> manyTill anyChar (string "\n") <*> getPosition

parseParens = Collection <$> pure Parens <*> parens lexer parseSepExprList <*> getPosition

parseSet = Collection <$> pure Set <*> braces lexer parseSepExprList <*> getPosition

parseVec = Collection <$> pure Vec <*> brackets lexer parseSepExprList <*> getPosition

parseSepExprList = try parseSepExprList1 <|> parseSingleton <|> return Nil

parseSingleton = Singleton <$> parseExpr

parseSepExprList1 = do
  x <- parseExpr
  sep <- parseSep
  xs <- parseSepExprList
  return $ Cons x sep xs

parseSep = choice
  [ Comma <$ lexeme lexer (char ',')
  , NewLine <$ lexeme lexer (newline)
  , Space <$ whiteSpace lexer
  ]

parseString = TaggedString <$> pure String <*> quotedString
  where
    quotedString :: Parsec String () String
    quotedString = do
      char '"'
      x <- many (try (try (string "\\\\") <|> string "\\\"") <|> fmap pure (noneOf "\""))
      char '"'
      return $ concat x

parseVar = TaggedString <$> pure Var <*> (identifier lexer <|> operator lexer)

parseMetadata = TaggedString <$> pure Metadata <* char '^' <*> identifier lexer

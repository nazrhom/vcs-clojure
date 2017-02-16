module Parser
    ( parseTop
    , parse
    , parseTest

    -- AST
    , Expr(..)
    , FormTy(..)
    , CollType(..)
    , Term(..)
    , Tag(..)
    , SepList(..)
    , Sep(..)
    ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char hiding (Space)

data SepList a = Nil | Singleton a | Cons a Sep (SepList a) deriving (Show, Eq)

data Sep = Space | Comma | NewLine deriving (Show, Eq)

data Expr = Special FormTy Expr
          | Dispatch Expr
          | Collection CollType (SepList Expr)
          | Term Term
          | Comment String
          deriving (Show, Eq)

-- ref: https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html
data FormTy = Quote | SQuote | UnQuote | SUnQuote | DeRef deriving (Show, Eq)

data CollType = Vec | Bindings | Map | Set | Array | Parens deriving (Show, Eq)

data Term = TaggedString Tag String
          deriving (Show, Eq)

data Tag = String | Metadata | Var  deriving (Show, Eq)

lexer = makeTokenParser javaStyle
  { identStart = alphaNum <|> oneOf "_':*-&\\"
  , identLetter = alphaNum <|> oneOf ":_.'-/^?!><*#\"\\" <|> satisfy isSymbol
  }

parseTop = whiteSpace lexer *> many parseExpr <* eof

parseExpr = lexeme lexer $ choice
  [ parseSpecial
  , parseDispatch
  , parseCollection
  , parseComment
  , Term <$> parseTerm
  ]

parseTerm = lexeme lexer $ parseTaggedString

parseCollection = choice [ parseParens, parseVec, parseSet ]

parseSpecial = choice [parseQuote, parseSQuote, try parseSUnQuote, parseUnQuote, parseDeRef]

parseQuote = Special <$> pure Quote <* char '\'' <*> parseExpr

parseSQuote = Special <$> pure SQuote <* char '`' <*> parseExpr

parseSUnQuote = Special <$> pure SUnQuote <* char '~' <* char '@' <*> parseExpr

parseUnQuote = Special <$> pure UnQuote <* char '~' <* optional (char '@') <*> parseExpr

parseDeRef = Special <$> pure DeRef <* char '@' <*> parseExpr

parseTaggedString = choice [parseString, parseVar, parseMetadata]


parseDispatch = Dispatch <$ char '#' <*> parseDispatchable
  where
    parseDispatchable = parseSet <|> parseDiscard <|> parseRegExp <|> parseParens <|> parseTaggedLit <|> parseMeta
    --- ref: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/
    -- parseParens covers the function marco
    -- parseTaggedLit covers the var macro (as identifiers can start with a quote ('))
    parseDiscard = Term <$> (TaggedString <$> pure Var <*> string "_")
    parseRegExp = Term <$> parseString
    parseTaggedLit = Term <$> parseVar
    parseMeta = Term <$> parseMetadata

parseComment = Comment <$ char ';' <*> manyTill anyChar (string "\n")

parseParens = Collection <$> pure Parens <*> parens lexer parseSepList

parseSet = Collection <$> pure Set <*> braces lexer parseSepList

parseVec = Collection <$> pure Vec <*> brackets lexer parseSepList

parseSepList = try parseSepList1 <|> parseSingleton <|> return Nil

parseSingleton = Singleton <$> parseExpr

parseSepList1 = do
  x <- parseExpr
  sep <- parseSep
  xs <- parseSepList
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

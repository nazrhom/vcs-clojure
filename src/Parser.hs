module Parser
    ( parseTop
    , parseTest
    ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char

data Expr = Parens [Expr]
          | Special Type Expr
          | Dispatch Expr
          | Collection CollType [Expr]
          | Term Term
          deriving (Show)

data Type = Quote | SQuote | DeRef deriving Show

data CollType = Vec | Bindings | Map | Set  deriving (Show)

data Term = TaggedString Tag String
          | Int Int
          | Nil
          deriving (Show)

data Tag = String | RegExp | Keyword | Metadata | Var  deriving (Show)



lexer = makeTokenParser javaStyle
  { reservedNames =
      [ "nil" ]
  , commentStart = ";"
  , commentEnd = "\n"
  , identStart = letter <|> oneOf "_:'"
  , identLetter = alphaNum <|> oneOf "_.'-/^:?!><*#"
  }

parseTop = whiteSpace lexer *> many parseExpr <* eof

parseExpr = lexeme lexer $ choice
  [ parseSpecial
  , parseParens
  , parseDispatch
  , parseCollection
  , Term <$> parseTerm
  ]

parseTerm = choice
  [ parseTaggedString
  , parseInt
  , parseNil
  ]

parseCollection = choice [ try parseVec, parseBindings, parseMap ]

parseSpecial = choice [parseQuote, parseSQuote, parseDeRef]

parseQuote = Special <$> pure Quote <* char '\'' <*> parseExpr

parseSQuote = Special <$> pure SQuote <* char '`' <*> parseExpr

parseDeRef = Special <$> pure DeRef <* char '~' <* optional (char '@') <*> parseExpr

parseTaggedString = choice [parseString, parseVar, parseKeyword, parseMetadata]

parseParens = Parens <$> parens lexer (many parseExpr)

parseDispatch = Dispatch <$ char '#' <*> parseDispatchable
  where
    parseDispatchable = parseSet <|> parseDiscard <|> parseRegExp <|> parseParens <|> parseTaggedLit

    --- ref: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/
    -- parseApp covers the function marco
    -- parse tagged lit covers the var macro (as identifiers can start with a quote')
    parseDiscard = Term <$> (TaggedString <$> pure Var <*> string "_")
    parseRegExp = Term <$> (TaggedString <$> pure RegExp <*> stringLiteral lexer)
    parseTaggedLit = Term <$> (TaggedString <$> pure Var <*> identifier lexer)
    parseSet = Collection <$> pure Set <*> braces lexer (many parseExpr)

parseVec = Collection <$> pure Vec <*> brackets lexer (many parseExpr)

parseMap = Collection <$> pure Map <*> braces lexer (fmap buildList parseElements)
  where
    parsemapEl = (,) <$> parseTerm <*> parseExpr
    parseElements = commaSep lexer parsemapEl
    buildList [] = []
    buildList ((k,v):rest) = (Term k):v:(buildList rest)

parseBindings = Collection <$> pure Bindings <*> brackets lexer (fmap buildList (commaSep lexer parsePair))
  where
    parsePair = (,) <$> parseExpr <*> parseExpr
    buildList [] = []
    buildList ((k,v):rest) = k:v:(buildList rest)

parseNil = Nil <$ reserved lexer "nil"

parseInt :: Parsec String () Term
parseInt = Int <$> (read <$> many1 digit)

-- parseList = Collection <$> pure List <* char '\'' <*> many parseExpr

parseString = TaggedString <$> pure String <*> quotedString
  where
    quotedString :: Parsec String () String
    quotedString = do
      char '"'
      x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
      char '"'
      return x

parseKeyword = TaggedString <$> pure Keyword <*> identifier lexer

parseVar = TaggedString <$> pure Var <*> (identifier lexer <|> operator lexer)

parseMetadata = TaggedString <$> pure Metadata <* char '^' <*> identifier lexer

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
    ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char

data Expr = Parens [Expr]
          | Special FormTy Expr
          | Dispatch Expr
          | Collection CollType [Expr]
          | Term Term
          | Comment String
          deriving (Show, Eq)

-- ref: https://8thlight.com/blog/colin-jones/2012/05/22/quoting-without-confusion.html
data FormTy = Quote | SQuote | UnQuote | SUnQuote deriving (Show, Eq)

data CollType = Vec | Bindings | Map | Set  deriving (Show, Eq)

data Term = TaggedString Tag String
          | Int Int
          | Nil
          deriving (Show, Eq)

data Tag = String | Metadata | Var  deriving (Show, Eq)

lexer = makeTokenParser javaStyle
  { reservedNames =
      [ "nil" ]
  , identStart = letter <|> oneOf "_':"
  , identLetter = alphaNum <|> oneOf "_.'-/^?!><*#"
  }

parseTop = whiteSpace lexer *> many parseExpr <* eof

parseExpr = lexeme lexer $ choice
  [ parseSpecial
  , parseParens
  , parseDispatch
  , parseCollection
  , parseComment
  , Term <$> parseTerm
  ]

parseTerm = choice
  [ parseTaggedString
  , parseInt
  , parseNil
  ]

parseCollection = choice [ try parseVec, parseBindings, parseMap ]

parseSpecial = choice [parseQuote, parseSQuote, try parseSUnQuote, parseUnQuote]

parseQuote = Special <$> pure Quote <* char '\'' <*> parseExpr

parseSQuote = Special <$> pure SQuote <* char '`' <*> parseExpr

parseSUnQuote = Special <$> pure SUnQuote <* char '~' <* char '@' <*> parseExpr

parseUnQuote = Special <$> pure UnQuote <* char '~' <* optional (char '@') <*> parseExpr

parseTaggedString = choice [parseString, parseVar, parseMetadata]

parseParens = Parens <$> parens lexer (many parseExpr)

parseDispatch = Dispatch <$ char '#' <*> parseDispatchable
  where
    parseDispatchable = parseSet <|> parseDiscard <|> parseRegExp <|> parseParens <|> parseTaggedLit

    --- ref: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/
    -- parseParens covers the function marco
    -- parseTaggedLit covers the var macro (as identifiers can start with a quote ('))
    parseDiscard = Term <$> (TaggedString <$> pure Var <*> string "_")
    parseRegExp = Term <$> parseString
    parseTaggedLit = Term <$> (TaggedString <$> pure Var <*> identifier lexer)
    parseSet = Collection <$> pure Set <*> braces lexer (many parseExpr)

parseComment = Comment <$ char ';' <*> manyTill anyChar (string "\n")

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

parseString = TaggedString <$> pure String <*> quotedString
  where
    quotedString :: Parsec String () String
    quotedString = do
      char '"'
      x <- many (string "\\\"" <|> fmap pure (noneOf "\""))
      char '"'
      return $ concat x

parseVar = TaggedString <$> pure Var <*> (identifier lexer <|> operator lexer)

parseMetadata = TaggedString <$> pure Metadata <* char '^' <*> identifier lexer

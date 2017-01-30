module Lib
    ( parseTop
    , parse
    ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Perm
import Data.Char
import qualified Data.Map as M
-- data Expr = Paren Expr
--           | Int Int
--           | Fn String [Expr]
--   deriving (Show, Eq)
--
--
-- parseTop :: Parsec String () [Expr]
-- parseTop = whiteSpace lexer *> many parseParen <* eof
--
-- parseExpr :: Parsec String () Expr
-- parseExpr = lexeme lexer $ choice [parseParen, parseInt, parseFn]
--
-- parseParen :: Parsec String () Expr
-- parseParen = Paren <$> parens lexer parseExpr
--
-- parseInt :: Parsec String () Expr
-- parseInt = Int <$> (read <$> many1 digit)
--
-- parseFn :: Parsec String () Expr
-- parseFn = Fn <$> (identifier lexer <|> operator lexer) <*> many parseExpr

lexer = makeTokenParser javaStyle
  { reservedNames =
      [ "defn", "ns", "nil", "defn-", "fn" ]
  , commentStart = ";"
  , commentEnd = "\n"
  , identStart = letter <|> oneOf "_^:'"
  , identLetter = alphaNum <|> oneOf "_.'-/^:?!><*"
  }

type Name = String
type DocString = Maybe String

data AST = Fn Name DocString AST AST
         | NameSpace Name DocString (Maybe AST)
         | App AST [AST]
         | Dispatch AST
         | Term Term
         | Map (M.Map String AST)
         -- Bunch these up in some Collection [AST] thing?
         | List [AST]
         | Vec [AST]
         | Set [AST]

         | Unknown String
         | Lam [(AST, AST)]
         deriving (Show)

data Term = Var Name
          | Int Int
          -- IDEA: TaggedString (String, Tag)
          -- data Tag = String | RegExp | Keyword | Metadata | Var
          | String String
          | RegExp String
          | Keyword String
          | Nil
          deriving (Show)

parseTop = whiteSpace lexer *> many parseExpr <* eof

parseDispatch = Dispatch <$ char '#' <*> parseDispatchable

parseDispatchable = parseSet <|> parseDiscard <|> parseRegexp <|> parseApp <|> parseTaggedLit
  where
    --- ref: https://yobriefca.se/blog/2014/05/19/the-weird-and-wonderful-characters-of-clojure/
    -- parseApp covers the function marco
    -- parse tagged lit covers the var macro (as identifiers can start with a quote')
    parseDiscard = Unknown <$> string "_"
    parseRegexp = Term <$> RegExp <$> parseString
    parseTaggedLit = Term <$> Var <$> identifier lexer
    parseSet = Set <$> braces lexer (many parseExpr)

parseExpr = lexeme lexer $ choice
  [ parseAST
  , Term <$> parseTerm
  , parseUnknown ]

parseAST = (parens lexer $ choice
  [ parseFn
  , parseLam
  , parseNameSpace
  , parseApp ] )
  <|> parseVec
  <|> parseMap
  <|> parseList

parseTerm = lexeme lexer $ choice
  [ parseVar
  , parseNil
  , String <$> parseString
  , parseKeyword ]

parseKeyword = Keyword <$> identifier lexer

parseFn = Fn <$ keyDef <*> identifier lexer <*> optionMaybe parseString <*> parseVec <*> parseExpr
  where
    keyDef = reserved lexer "defn" <|> reserved lexer "defn-"

parseLam = Lam <$ reserved lexer "fn" <*> many parseCases
  where parseCases = (,) <$ optional (char '(') <*> parseVec <*> parseExpr <* optional (char ')')

parseNameSpace = NameSpace <$ reserved lexer "ns" <*> identifier lexer <*> optionMaybe parseString <*> optionMaybe parseExpr

parseApp = App <$> parseExpr <*> many parseExpr

parseList = List <$ char '\'' <*> many parseExpr

parseUnknown :: Parsec String () AST
parseUnknown = Unknown <$> (manyTill anyChar (try (char '\n')))

parseString = lexeme lexer $ between (char '"') (char '"') $ manyTill anyChar (lookAhead (string "\""))

parseMap = Map <$> braces lexer (fmap M.fromList parseElements)
  where
    parsemapEl = (,) <$> identifier lexer <*> parseExpr
    parseElements = commaSep lexer parsemapEl

parseVec = Vec <$> brackets lexer (many parseExpr)

parseVar = Var <$> identifier lexer

parseNil = Nil <$ reserved lexer "nil"


parseInt :: Parsec String () Term
parseInt = Int <$> (read <$> many1 digit)
-- parseVector = brackets lexer $
-- data AST = Node Type Rest Children
--
-- data Rest = String
--
-- type Type = String
-- type Name = String
-- type Children = Maybe AST
--
-- parseExpr = whiteSpace lexer *> many parseNode <* eof
--
-- -- parseNode = Node <$> braces lexer
--
-- parseNode = braces lexer
--
-- opField = (:) <$> semicolon lexer <*> reserved "op" lexer

module Main where

import Options.Applicative
import Data.Monoid

import Util.Test
import Util.DiffParser

main :: IO ()
main = do
  op <- execParser optsHelper
  case op of
    Conflict f -> processConflictFolder f >> return ()
    Patch s d j _ -> patchFiles s d j
    Preprocess f  -> runMinify f

data Opts =
  Conflict {
    folder :: String
  } |
  Patch {
    srcFile :: String
  , dstFile :: String
  , jsonOutput :: Maybe String
  , printAll :: Bool
  } |
  Preprocess {
    prepFolder :: String
  }

opts :: Parser Opts
opts = (
  Patch <$> strOption
    (  long "source"
    <> short 's'
    <> metavar "SRC_TARGET"
    <> help "Source file"
    )
  <*> strOption
    (  long "destination"
    <> short 'd'
    <> metavar "DST_TARGET"
    <> help "Destination file"
    )
  <*> optional (strOption
    (  long "json-output"
    <> short 'j'
    <> metavar "OUT_TARGET"
    <> help "Output file"
    ))
  <*> switch
    ( long "all"
    <> short 'a'
    <> help "Print all patches")
  ) <|> (
  Conflict <$> strOption
    (  long "folder"
    <> short 'f'
    <> metavar "FOLDER"
    <> help "Folder to process"
    )
  ) <|> (
   Preprocess <$> strOption
     ( long "preprocess"
     <> short 'p'
     <> metavar "PREPROCESS"
     <> help "Folder to preprocess"
     )
  )

optsHelper :: ParserInfo Opts
optsHelper = info (helper <*> opts)
  ( fullDesc
  <> progDesc "Clojure parser in Haskell"
  )

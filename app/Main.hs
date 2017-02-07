module Main where

import Parser
import PrettyPrint

import System.IO
import Options.Applicative

main :: IO ()
main = do
  opts <- execParser optsHelper
  f <- readFile (inFile opts)
  case parse parseTop (inFile opts) f of
    Left err -> putStrLn $ show err
    Right t -> do
      putStrLn $ "Parse Output: \n" ++ show t
      setHandle (outFile opts) $ flip hPutStrLn $ ppTop t
      case parse parseTop "" (ppTop t) of
        Left err -> putStrLn $ "Couldn't reparse" ++ show err
        Right t1 -> putStrLn $ "Parse == Reparse " ++ show (t == t1)

data Opts = Opts
  { outFile :: Maybe String
  , inFile :: String
  }

setHandle :: Maybe String -> (Handle -> IO a) -> IO a
setHandle Nothing act = act stdout
setHandle (Just path) act = withFile path WriteMode act

opts :: Parser Opts
opts = Opts
  <$> optional
    ( strOption
      (  long "output"
      <> short 'o'
      <> metavar "OUT_TARGET"
      <> help "Output file"
      )
    )
  <*> strOption
    (  long "input"
    <> short 'i'
    <> metavar "IN_TARGET"
    <> help "Input file"
    )

optsHelper :: ParserInfo Opts
optsHelper = info (helper <*> opts)
  ( fullDesc
  <> progDesc "Clojure parser in Haskell"
  )

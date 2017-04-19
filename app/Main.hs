module Main where

import System.IO
import Options.Applicative
import Data.Monoid
import Data.List (sortBy)
import Data.Ord (comparing)

import Parser
import PrettyPrint

import Diff
import Lang
import Apply
import Multirec
import Cost

main :: IO ()
main = do
  opts <- execParser optsHelper
  s <- readFile (srcFile opts)
  src <- parseAndPop (srcFile opts) s
  putStrLn $ "Parse Output: \n" ++ show src

  d <- readFile (dstFile opts)
  dst <- parseAndPop (dstFile opts) d

  let almus = diffAlmu M (toSing src) (toSing dst)
  let patches = map (flip applyAlmu (toSing src)) almus
  let almusCost = sortBy (comparing costAlmu) almus
  let worst = last almusCost
  let best = head almusCost
  putStrLn $ "Found " ++ show (length almus) ++ " patches."
  putStrLn $ "Worst patch has a cost of : " ++ show (costAlmu worst)
  putStrLn $ "Corresponding to \n" ++ show worst
  putStrLn $ "Lowest cost found: " ++ show (costAlmu best)
  putStrLn $ "Corresponding to \n" ++ show best
  putStrLn $ "All the same? " ++ show (allTheSame patches)
  putStrLn $ show $ applyAlmu (head almus) (toSing src)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

parseAndPop :: String -> String -> IO Expr
parseAndPop name src = case parse parseTop name src of
  Left err -> error $ show err
  Right s' -> return $ head s'

data Opts = Opts
  {
    srcFile :: String
  , dstFile :: String
  }

setHandle :: Maybe String -> (Handle -> IO a) -> IO a
setHandle Nothing act = act stdout
setHandle (Just path) act = withFile path WriteMode act

opts :: Parser Opts
opts = Opts
  <$> strOption
    (  long "soure"
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

optsHelper :: ParserInfo Opts
optsHelper = info (helper <*> opts)
  ( fullDesc
  <> progDesc "Clojure parser in Haskell"
  )

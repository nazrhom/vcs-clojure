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
import Disjoint

import Data.Maybe

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

choose' :: Almu u v -> Int -> [Almu u v] -> Almu u v
choose' curr c [] = curr
choose' curr c (p:ps)
  | c <= costAlmu p = choose' curr c ps
  | otherwise       = choose' p (costAlmu p) ps

choose :: [Almu u v] -> Almu u v
choose (x:xs) = choose' x (costAlmu x) xs
choose []     = error "boom"

computePatch :: Expr -> Expr -> Almu (Le Expr) (Le Expr)
computePatch x y
  = let almus = diffAlmu M (toSing x) (toSing y)
     in choose almus

getFiles :: IO (Expr , Expr , Expr)
getFiles = do
  let src  = "test/conflicts/manual/head-safehead-disj/head.clj"
      dst1 = "test/conflicts/manual/head-safehead-disj/safehead-1.clj"
      dst2 = "test/conflicts/manual/head-safehead-disj/safehead-2.clj"
  fs  <- parseAndRead src
  fd1 <- parseAndRead dst1
  fd2 <- parseAndRead dst2
  return (fs , fd1 , fd2)

testDisjoint :: (Expr, Expr, Expr) -> IO ()
testDisjoint (o, a, b) = do
  let p1 = computePatch o a
      p2 = computePatch o b
      disj = disjoint p1 p2
  putStrLn $ "disjoint? " ++ show disj
  putStrLn $ "check:" ++ show (applyAlmu p1 (fromJust (applyAlmu p2 (toSing o))) == applyAlmu p2 (fromJust (applyAlmu p1 (toSing o))))

parseAndRead :: String -> IO Expr
parseAndRead fname
  = readFile fname >>= parseAndPop ""

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

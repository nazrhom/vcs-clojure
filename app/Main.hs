{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Options.Applicative
import Data.Monoid
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe
import Data.Proxy

import Clojure.Parser
import Clojure.Lang
import Clojure.PrettyPrint

import VCS.Diff
import VCS.Apply
import VCS.Multirec
import VCS.Cost
import VCS.Disjoint

import Util.PPPatch

import Oracle.Oracle

import Util.UnixDiff

main :: IO ()
main = do
  opts <- execParser optsHelper

  s <- readFile (srcFile opts)
  src <- parseFile (srcFile opts) s

  d <- readFile (dstFile opts)
  dst <- parseFile (dstFile opts) d

  let diff3 = preprocess s d
  let oracle = (DiffOracle $ buildOracle diff3) <°> NoDupBranches

  let almu = computePatch oracle src dst
  putStrLn $ "Best patch " ++ show almu
  -- let almus = diffAlmu oracle (toSing src) (toSing dst)
  -- let patches = map (flip applyAlmu (toSing src)) almus
  -- let almusCost = sortBy (comparing costAlmu) almus
  -- let worst = last almusCost
  -- let best = head almusCost
  -- putStrLn $ "Found " ++ show (length almus) ++ " patches."
  -- putStrLn $ "Worst patch has a cost of : " ++ show (costAlmu worst)
  -- putStrLn $ "Corresponding to \n" ++ show worst
  -- putStrLn $ "Lowest cost found: " ++ show (costAlmu best)
  -- putStrLn $ "Corresponding to \n" ++ show best
  -- putStrLn $ "All the same? " ++ show (allTheSame patches)
  -- putStrLn $ show $ applyAlmu (head almus) (toSing src)

processConflictFolder :: IO ()
processConflictFolder = do
  a1 <- readFile "A1.clj"
  a <- parseFile "A1.clj" a1

  b1 <- readFile "B1.clj"
  b <- parseFile "B1.clj" b1

  o1 <- readFile "O1.clj"
  o <- parseFile "O1.clj" o1

  let diffOA = preprocess o1 a1
  let oracleOA = (DiffOracle $ buildOracle diffOA) <°> NoDupBranches

  let diffOB = preprocess o1 b1
  let oracleOB = (DiffOracle $ buildOracle diffOB) <°> NoDupBranches

  let almuOA = computePatch oracleOA o a
  let almuOB = computePatch oracleOB o b

  putStrLn $ "Patch O-A: " ++ show almuOA
  putStrLn $ "Patch O-B: " ++ show almuOB



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

computePatches :: (MonadOracle o m) =>
      o -> Expr -> Expr -> m (Almu (Le Expr) (Le Expr))
computePatches o x y = diffAlmu o (toSing x) (toSing y)

computePatch :: (MonadOracle o []) =>
      o -> Expr -> Expr -> Almu (Le Expr) (Le Expr)
computePatch o x y
  = let almus = computePatches o x y
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

-- test :: IO ()
-- test = do
--   (o, a, b) <- getFiles
--   let almu1 = computePatches o a
--       almu2 = computePatches o b
--   putStrLn $ "Length almu1 " ++ show (length almu1)
--   putStrLn $ "Length almu2 " ++ show (length almu2)
--
-- testDisjoint :: (Expr, Expr, Expr) -> IO ()
-- testDisjoint (o, a, b) = do
--   let p1 = computePatch o a
--       p2 = computePatch o b
--       disj = disjoint p1 p2
--   putStrLn $ "disjoint? " ++ show disj
--   putStrLn $ "check:" ++ show (applyAlmu p1 (fromJust (applyAlmu p2 (toSing o))) == applyAlmu p2 (fromJust (applyAlmu p1 (toSing o))))

parseAndRead :: String -> IO Expr
parseAndRead fname
  = readFile fname >>= parseFile ""

parseFile :: String -> String -> IO Expr
parseFile name src = case parse parseTop name src of
  Left err -> error $ show err
  Right s' -> return s'



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

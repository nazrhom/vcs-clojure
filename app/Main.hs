{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import System.Exit
import Options.Applicative
import Data.Monoid
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe
import Data.Proxy
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B

import Clojure.Parser
import Clojure.Lang
import Clojure.PrettyPrint

import VCS.Diff
import VCS.Apply
import VCS.Multirec
import VCS.Cost
import VCS.Disjoint
import VCS.Compatible

import Util.PPPatch
import Util.ToJSON
import Util.Treeview

import Oracle.Oracle

import Util.UnixDiff

import Patches.Diff3

main :: IO ()
main = do
  opts <- execParser optsHelper

  case (folder opts) of
    Just folder -> processConflictFolder folder
    Nothing     -> patchFiles (srcFile opts) (dstFile opts) (jsonOutput opts)



printPatchesWithCost :: [Almu u v] -> IO ()
printPatchesWithCost almus = mapM_ printPatchWithCost almus

printPatchWithCost :: Almu u v -> IO ()
printPatchWithCost almu = do
  putStrLn $ "Cost: " ++ show (costAlmu almu)
  putStrLn (show $ almu)

patchFiles :: String -> String -> Maybe String -> IO ()
patchFiles srcFile dstFile jsonOut = do
  s <- readFile srcFile
  src <- parseFile srcFile s

  d <- readFile dstFile
  dst <- parseFile dstFile d

  putStrLn $ show src
  putStrLn $ show dst
  let diff3 = buildDelInsMap $ preprocessGrouped s d
  let diff3_plain = preprocess s d
  let copyMaps = buildCopyOracle diff3_plain

  putStrLn $ show $ diff3
  putStrLn $ show $ copyMaps
  -- putStrLn $ show $ diff3_plain
  let gdiff = solveConflicts diff3 copyMaps src dst
  putStrLn $ show $ gdiff
  -- putStrLn $ show $ gdiff
  let oracle = (DiffOracle gdiff <°> NoDupBranches)
  let almus = computePatches oracle src dst

  case almus of
    []    -> error "boom"
    almus -> do
      putStrLn $ "Found " ++ show (length almus) ++ " patches"
      case (jsonOut) of
        Nothing -> return ()
        Just path -> do
          let almusCost = sortBy (comparing costAlmu) almus
          printPatchWithCost (head almusCost)
          B.writeFile path $ encodePretty (head almusCost)



processConflictFolder :: String -> IO ()
processConflictFolder folder = do
  a1 <- readFile $ folder ++ "/A1.clj"
  a <- parseFile "A1.clj" a1

  b1 <- readFile $ folder ++ "/B1.clj"
  b <- parseFile "B1.clj" b1

  o1 <- readFile $ folder ++ "/O1.clj"
  o <- parseFile "O1.clj" o1

  let delInsOA = buildDelInsMap $ preprocessGrouped o1 a1
  let delInsOB = buildDelInsMap $ preprocessGrouped o1 b1

  let cpOA = buildCopyMaps $ preprocess o1 a1
  let cpOB = buildCopyMaps $ preprocess o1 b1

  let diffOA = solveConflicts delInsOA cpOA o a
  let diffOB = solveConflicts delInsOB cpOB o b

  let oracleOA = (DiffOracle diffOA) <°> NoDupBranches

  let oracleOB = (DiffOracle diffOB) <°> NoDupBranches

  let almuOA = computePatch oracleOA o a
  let almuOB = computePatch oracleOB o b
  let disj = disjoint almuOA almuOB
  let comp = compatible almuOA almuOB
  putStrLn $ show disj
  putStrLn $ show comp
  if comp
    then exitSuccess
    else exitWith (ExitFailure 1001)
  -- putStrLn $ "Patch O-A: " ++ show almuOA
  -- putStrLn $ "Patch O-B: " ++ show almuOB


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
      o -> Expr -> Expr -> m (Almu (ToSing Expr) (ToSing Expr))
computePatches o x y = diffAlmu o (toSing x) (toSing y)

computePatch :: (MonadOracle o []) =>
      o -> Expr -> Expr -> Almu (ToSing Expr) (ToSing Expr)
computePatch o x y
  = let almus = computePatches o x y
     in choose almus

getFiles :: IO (Expr , Expr , Expr)
getFiles = do
  let src  = "test/conflicts/manual/head-safehead-disj/head.clj"
      dst1 = "test/conflicts/manual/head-safehead-disj/safehead-1.clj"
      dst2 = "test/conflicts/manual/head-safehead-disj/safehead-2.clj"
  fs  <- readAndParse src
  fd1 <- readAndParse dst1
  fd2 <- readAndParse dst2
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

readAndParse :: String -> IO Expr
readAndParse fname
  = readFile fname >>= parseFile ""

parseFile :: String -> String -> IO Expr
parseFile name src = case parse parseTop name src of
  Left err -> error $ show err
  Right s' -> return s'



data Opts = Opts
  {
    srcFile :: String
  , dstFile :: String
  , jsonOutput :: Maybe String
  , printAll :: Bool
  , folder :: Maybe String
  }

setHandle :: Maybe String -> (Handle -> IO a) -> IO a
setHandle Nothing act = act stdout
setHandle (Just path) act = withFile path WriteMode act

opts :: Parser Opts
opts = Opts
  <$> strOption
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
  <*> optional (strOption
    (  long "folder"
    <> short 'f'
    <> metavar "FOLDER"
    <> help "Folder to process"
    ))

optsHelper :: ParserInfo Opts
optsHelper = info (helper <*> opts)
  ( fullDesc
  <> progDesc "Clojure parser in Haskell"
  )

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import System.IO
import System.Exit
import Options.Applicative
import Data.Monoid
import Data.Proxy ()
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B

import Debug.Trace

import Language.Clojure.Parser
import Language.Clojure.Lang
import Language.Clojure.Cost


import VCS.Diff
import VCS.Multirec
import VCS.Cost
import VCS.Disjoint
import VCS.Compatible

import Util.ToJSON
import Util.UnixDiff
import Util.DiffParser

import Oracle.Oracle

main :: IO ()
main = do
  op <- execParser optsHelper
  case op of
    Conflict f -> processConflictFolder f
    Patch s d j _ -> patchFiles s d j
    Preprocess f  -> runMinify f

printPatchesWithCost :: [Almu u v] -> IO ()
printPatchesWithCost almus = mapM_ printPatchWithCost almus

printPatchWithCost :: Almu u v -> IO ()
printPatchWithCost almu = do
  putStrLn $ "Cost: " ++ show (costAlmu almu)
  putStrLn (show $ almu)

estimateParams :: Expr -> Expr -> (Int, Int)
estimateParams e1 e2 = (initialCost, initialCost `div` 5)
  where
    initialCost = abs (costExpr e1 - costExpr e2)

patchFiles :: String -> String -> Maybe String -> IO ()
patchFiles srcFile dstFile jsonOut = do
  s <- readFile srcFile
  src <- parseFile srcFile s

  d <- readFile dstFile
  dst <- parseFile dstFile d

  -- putStrLn $ show src
  -- putStrLn $ show dst
  let diff3 = preprocessGrouped s d
  let delInsMap = buildDelInsMap diff3
  let diff3_plain = preprocess s d
  let copyMaps = buildCopyMaps diff3_plain
  let gdiff = solveConflicts delInsMap copyMaps src dst
  let oracle = (DiffOracle gdiff <°> NoDupBranches)
  let (initialCost, incr) = estimateParams src dst
  putStrLn $ show initialCost
      -- oracle = (OldDiffOracle diff3 <°> NoDupBranches)
  let almu = computePatchesBounded oracle initialCost incr src dst
  -- putStrLn $ show $ diff3
  -- putStrLn $ show $ copyMaps
  -- putStrLn $ show $ diff3_plain

  -- putStrLn $ show $ gdiff

  -- putStrLn $ show $ gdiff
  case (jsonOut) of
    Nothing -> return ()
    Just path -> do
      printPatchWithCost almu
      B.writeFile path $ encodePretty almu

processConflictFolder :: String -> IO ()
processConflictFolder folder = do
  a1 <- readFile $ folder ++ "A1.clj"
  a <- parseFile "A1.clj" a1

  b1 <- readFile $ folder ++ "B1.clj"
  b <- parseFile "B1.clj" b1

  o1 <- readFile $ folder ++ "O1.clj"
  o <- parseFile "O1.clj" o1

  let delInsOA = buildDelInsMap $ preprocessGrouped o1 a1
  let delInsOB = buildDelInsMap $ preprocessGrouped o1 b1

  let cpOA = buildCopyMaps $ preprocess o1 a1
  let cpOB = buildCopyMaps $ preprocess o1 b1

  let diffOA = solveConflicts delInsOA cpOA o a
  let diffOB = solveConflicts delInsOB cpOB o b

  let oracleOA = (DiffOracle diffOA) <°> NoDupBranches
  let oracleOB = (DiffOracle diffOB) <°> NoDupBranches

  let almuOA = computePatchesBounded oracleOA 25 5 o a
  let almuOB = computePatchesBounded oracleOB 25 5 o b
  -- putStrLn $ show almuOA
  -- putStrLn $ show almuOB

  let disj = disjoint almuOA almuOB
  let sDisj = structurallyDisjoint almuOA almuOB
  let comp = compatible almuOA almuOB
  let sComp = structurallyCompatible almuOA almuOB

  putStrLn $ show disj
  putStrLn $ show sDisj
  putStrLn $ show comp
  putStrLn $ show sComp

  let res = encode disj sDisj comp sComp
  -- We encode the results in a binary number
  exitWith (ExitFailure res)
  -- putStrLn $ "Patch O-A: " ++ show almuOA
  -- putStrLn $ "Patch O-B: " ++ show almuOB

encode :: Bool -> Bool -> Bool -> Bool -> Int
encode a b c d = 1 +
    (convert a) * 2^3 +
    (convert b) * 2^2 +
    (convert c) * 2^1 +
    (convert d) * 2^0
  where
  convert True  = 1
  convert False = 0

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

choose' :: Almu u v -> Int -> [Almu u v] -> Almu u v
choose' curr _ [] = curr
choose' curr c (p:ps)
  | c <= costAlmu p = choose' curr c ps
  | otherwise       = choose' p (costAlmu p) ps

choose :: [Almu u v] -> Almu u v
choose (x:xs) = choose' x (costAlmu x) xs
choose []     = error "boom"

computePatches :: (MonadOracle o m) =>
      o -> Expr -> Expr -> m (Almu (ToSing Expr) (ToSing Expr))
computePatches o x y = diffAlmu o 10000 (toSing x) (toSing y)

computePatchBounded :: (MonadOracle o S.Seq) =>
      o -> Int -> Int -> Int -> Expr -> Expr -> (Almu (ToSing Expr) (ToSing Expr))
computePatchBounded o start incr r x y =
  if (S.null almus)
  then trace ("empty" ++ show start) recur
  else
    if anyBounded start (toList almus)
    then trace ("found " ++ show (S.length almus)) (choose (toList almus))
    else trace ("greater" ++ (show $ S.length almus)) recur
  where
    anyBounded :: Int -> [Almu u v] -> Bool
    anyBounded _ [] = False
    anyBounded i (a:as) = if costAlmu a <= i then True else anyBounded i as

    almus = diffAlmu o start (toSing x) (toSing y)
    recur = computePatchBounded o (start+(incr*r)) incr (r+1) x y

computePatchesBounded :: (MonadOracle o S.Seq) =>
      o -> Int -> Int -> Expr -> Expr -> (Almu (ToSing Expr) (ToSing Expr))
computePatchesBounded o s i x y = computePatchBounded o s i 1 x y

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

setHandle :: Maybe String -> (Handle -> IO a) -> IO a
setHandle Nothing act = act stdout
setHandle (Just path) act = withFile path WriteMode act

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

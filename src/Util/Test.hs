{-# LANGUAGE FlexibleContexts #-}
module Util.Test where

import System.IO
import System.Exit
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B

import Oracle.Oracle

import VCS.Diff
import VCS.Multirec
import VCS.Cost
import VCS.Disjoint
import VCS.Compatible

import Util.ToJSON
import Util.UnixDiff

import Language.Clojure.Parser
import Language.Clojure.Cost
import Language.Clojure.Lang

import Debug.Trace

data TestResult = Timeout FilePath | Error FilePath |
  TestResult  { disj :: Bool
              , sDisj :: Bool
              , comp :: Bool
              , sComp :: Bool
              , tPath :: FilePath }
  deriving (Show, Eq)

processConflictFolder :: String -> IO TestResult
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
  let (initialCostOA, incrOA) = estimateParams o a
  let (initialCostOB, incrOB) = estimateParams o b


  let almuOA = computePatchesBounded oracleOA initialCostOA incrOA o a
  let almuOB = computePatchesBounded oracleOB initialCostOB incrOB o b
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

  let res = encode disj sDisj comp sComp folder
  -- We encode the results in a binary number
  return res
  -- putStrLn $ "Patch O-A: " ++ show almuOA
  -- putStrLn $ "Patch O-B: " ++ show almuOB


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

encode :: Bool -> Bool -> Bool -> Bool -> String -> TestResult
encode disj sdisj comp scomp folder = TestResult {
     disj = disj
   , sDisj = sdisj
   , comp = comp
   , sComp = scomp
   , tPath = folder
   }




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
  then recur
  else choose (toList almus)
  where
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

printPatchesWithCost :: [Almu u v] -> IO ()
printPatchesWithCost almus = mapM_ printPatchWithCost almus

printPatchWithCost :: Almu u v -> IO ()
printPatchWithCost almu = do
 putStrLn $ "Cost: " ++ show (costAlmu almu)
 putStrLn (show $ almu)

estimateParams :: Expr -> Expr -> (Int, Int)
estimateParams e1 e2 = (toNat initialCost, toNat (initialCost `div` 5))
 where
   initialCost = abs (costExpr e1 - costExpr e2)
   toNat 0 = 1
   toNat n = n

readAndParse :: String -> IO Expr
readAndParse fname
 = readFile fname >>= parseFile ""

parseFile :: String -> String -> IO Expr
parseFile name src = case parse parseTop name src of
 Left err -> error $ show err
 Right s' -> return s'

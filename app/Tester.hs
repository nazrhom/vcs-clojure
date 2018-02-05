{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Process
import System.Directory
import System.Exit
import Control.Monad
import Options.Applicative
import Data.Monoid
import System.Timeout
import System.FilePath
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Char
import Numeric

import Language.Clojure.AST
import Language.Clojure.Parser
import Language.Clojure.Lang

import VCS.Multirec
import VCS.Disjoint
import VCS.Cost
import VCS.Diff

import Oracle.Oracle
import Util.UnixDiff

import Debug.Trace

testPath :: FilePath
testPath = "test/conflicts2"

resultPath :: FilePath
resultPath = "/Users/giovannigarufi/Developement/thesis/th-vcs-clojure/test/results/"

executablePathRoot :: FilePath
executablePathRoot = ".stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/th-vcs-clojure-exe/th-vcs-clojure-exe"

executablePath = "/Users/giovannigarufi/Developement/thesis/th-vcs-clojure/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/th-vcs-clojure-exe/th-vcs-clojure-exe"

timeout_time :: Int
timeout_time = (minutesToMicro 1)

minutesToMicro :: Int -> Int
minutesToMicro i = i * (6 * 10 ^ 7)

main :: IO ()
main = do
  opts <- execParser optsHelper

  dirs <- listDirectory testPath
  let dirsP = map (\d -> testPath ++ "/" ++ d) dirs
  actual_dirs <- filterM doesDirectoryExist dirsP
  results <- mapM (flip withCurrentDirectory checkPredicates) actual_dirs

  let timeouts = filter isTimeout results
  let completed = filter (\b -> not (isTimeout b)) results

  let disj_  = filter (\t -> disj t == True) completed
      sDisj_ = filter (\t -> sDisj t == True) completed
      comp_  = filter (\t -> comp t == True) completed
      sComp_ = filter (\t -> sComp t == True) completed

  putStrLn $ "Number of Tests " ++ show (length results)
  putStrLn $ "Number of Timeouts " ++ show (length timeouts)

  putStrLn $ "Disjoint: " ++ show (length disj_)
  putStrLn $ "Structurally-Disjoint: " ++ show (length sDisj_)
  putStrLn $ "Compatible: " ++ show (length comp_)
  putStrLn $ "Structurally-Compatible: " ++ show (length sComp_)
  when (not (dry opts)) (writeResults results)


isTimeout :: TestResult -> Bool
isTimeout (Timeout _) = True
isTimeout _           = False

writeResults :: [TestResult] -> IO ()
writeResults res = do
  let splitRes = splitInFolders res
  mapM_ writeTables splitRes

  where
    baseFolder s = head (splitPath s)

simplifyName :: TestResult -> String
simplifyName res = intercalate "-" $ reverse $ drop 2 $ reverse $ splitOn "-" (last $ splitPath (extractPath res))

splitInFolders :: [TestResult] -> [(FilePath, [TestResult])]
splitInFolders r = map (\xs ->
    (simplifyName (head xs), xs)) groups
  where
  groups = (groupBy
    (\x y ->
      simplifyName x == simplifyName y)
    r)

extractPath :: TestResult -> FilePath
extractPath (Timeout a) = a
extractPath tr          = tPath tr

liftMaybe :: (TestResult -> Bool) -> TestResult -> Maybe Bool
liftMaybe f (Timeout _) = Nothing
liftMaybe f a           = Just $ f a

writeTables :: (FilePath, [TestResult]) -> IO ()
writeTables (path, result) = do
  let rDisj = liftMaybe disj
      rSDisj = liftMaybe sDisj
      rComp = liftMaybe comp
      rSComp = liftMaybe sComp

  writeTableFor rDisj (toConflictResultFolder "Disj") result
  writeTableFor rSDisj (toConflictResultFolder "Struct-Disj") result
  writeTableFor rComp (toConflictResultFolder "Comp") result
  writeTableFor rSComp (toConflictResultFolder "Struct-Comp") result
  where
  toConflictResultFolder name =
    resultPath ++ path ++ "/" ++ name ++ ".md"

writeTableFor :: (TestResult -> Maybe Bool)
              -> FilePath -> [TestResult] -> IO ()
writeTableFor f path result = do
  writeFile path (mkTableFor f result)


mkTableFor :: (TestResult -> Maybe Bool) -> [TestResult] -> String
mkTableFor takeRes res = headers ++ "\n" ++ border ++ "\n" ++ values
  where
    headers = foldl addHeader "" res
    border  = foldl addBorder "" res
    values  = foldl addResults "" res

    addHeader :: String -> TestResult -> String
    addHeader soFar r  =
      soFar ++ "| " ++ simplify (extractPath r) ++ " | "

    addBorder :: String -> TestResult -> String
    addBorder soFar x =
      soFar ++ "|" ++ replicate 13 '-' ++ "|"

    addResults :: String -> TestResult -> String
    addResults soFar r =
      soFar ++ "|" ++ replicate 6 ' ' ++ convert (takeRes r)
        ++ replicate 6 ' ' ++ "|"
    convert r = case r of
      Just True  -> "T"
      Just False -> "F"
      Nothing    -> "X"

    simplify :: FilePath -> String
    simplify h = intercalate "-" $ map (take 5) (drop (length split - 2) split)
      where
        split = splitOn "-" (last (splitPath h))

runDiff :: IO Bool
runDiff = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in diff " ++ cwd
  ph <- spawnProcess executablePath ["-s", "O1.clj", "-d", "B1.clj"]
  result <- timeout timeout_time (waitForProcess ph)
  case result of
    Nothing -> do
      terminateProcess ph
      putStrLn $ "Process Timed out"
      return False
    Just exitcode -> do
      putStrLn $ "Process terminated with exitcode " ++ show exitcode
      return True

data TestResult = Timeout FilePath |
  TestResult  { disj :: Bool
              , sDisj :: Bool
              , comp :: Bool
              , sComp :: Bool
              , tPath :: FilePath }
  deriving (Show, Eq)

checkPredicates :: IO TestResult
checkPredicates = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in " ++ cwd
  ph <- spawnProcess executablePath ["-f", cwd ++ "/"]
  result <- timeout timeout_time (waitForProcess ph)
  case result of
    Nothing -> do
      terminateProcess ph
      putStrLn $ "Process Timed out"
      return $ Timeout cwd
    Just (ExitFailure 0) -> return $ Timeout cwd -- errors count as timeouts
    Just (ExitFailure c) -> return $ decode c cwd

decode :: Int -> FilePath -> TestResult
decode i p = TestResult {
            disj=disj
          , sDisj = sDisj
          , comp = comp
          , sComp = sComp
          , tPath = p
    }
  where
    int2b '1' = True
    int2b '0' = False
    int2b i = error $ "Can not decode " ++ show i

    disj = int2b $ numbers !! 0
    sDisj = int2b $ numbers !! 1
    comp = int2b $ numbers !! 2
    sComp = int2b $ numbers !! 3

    numbers = conv2Bin (i - 1)

conv2Bin :: Int -> String
conv2Bin x = if length ans < 4
    then reverse $ take 4 $ (reverse ans) ++ (repeat '0')
    else ans
  where
  ans = Numeric.showIntAtBase 2 Data.Char.intToDigit x ""

checkNoConflicy :: IO (Maybe Bool)
checkNoConflicy = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in " ++ cwd
  conflicts <- checkConflict "O1.clj" "A1.clj"
  let status = all (all (==NoConflict)) conflicts
  if status
  then return $ Just status
  else do
    putStrLn $ show conflicts
    return $ Just status

checkConflict :: String -> String -> IO [[MbMoveConflict]]
checkConflict srcFile dstFile = do
  s <- readFile srcFile
  d <- readFile dstFile
  src <- parseFile "" s
  dst <- parseFile "" d
  let cp = buildCopyMaps (preprocess s d)
  let diff3 = buildDelInsMap $ preprocessGrouped s d
  putStrLn $ "cp" ++ show cp
  putStrLn $ "diff3" ++ show diff3
  return $ checkCopyMaps cp src dst

parseFile :: String -> String -> IO Expr
parseFile name src = case parse parseTop name src of
  Left err -> error $ show err
  Right s' -> return s'



data Opts = Opts {
  dry :: Bool
}

opts :: Parser Opts
opts = Opts <$> switch
    ( long "dry"
    <> short 'd'
    <> help "Dry run")

optsHelper :: ParserInfo Opts
optsHelper = info (helper <*> opts)
  ( fullDesc
  <> progDesc "Clojure parser in Haskell"
  )
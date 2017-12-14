{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Process
import System.Directory
import System.Exit
import Control.Monad
import System.Timeout
import System.FilePath
import Data.List
import Data.List.Split
import Data.Time.Clock

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
testPath = "test/conflicts/mined"

resultPath = "/Users/giovannigarufi/Developement/thesis/th-vcs-clojure/test/results/"
executablePathRoot :: FilePath
executablePathRoot = ".stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/th-vcs-clojure-exe/th-vcs-clojure-exe"


executablePath = "../../../../" ++ executablePathRoot

timeout_time :: Int
timeout_time = 600000

minutesToMicro :: Int -> Int
minutesToMicro i = i * (6 * 10 ^ 7)

main :: IO ()
main = do
  dirs <- listDirectory testPath
  let dirsP = map (\d -> testPath ++ "/" ++ d) dirs
  actual_dirs <- filterM doesDirectoryExist dirsP
  results <- mapM (flip withCurrentDirectory checkDisjointness) actual_dirs
  writeResults results
  let successes = filter (\b -> snd b == Just True) results
  let failures  = filter (\b -> snd b == Just False) results
  let timeouts = filter  (\b -> snd b == Nothing) results
  putStrLn $ "Succeded in " ++ show (length successes)
  putStrLn $ "\nFailed in " ++ show (length failures)
  putStrLn $ "\nWith " ++ show (length timeouts) ++  " timeouts "

writeResults :: [(String, Maybe Bool)] -> IO ()
writeResults res = do
  let splitRes = splitInFolders res
  traceM (show $ length splitRes)
  mapM_ writeTable splitRes

  where
    baseFolder s = head (splitPath s)
    splitInFolders r = map (\xs -> (takeBaseName (fst (head xs)), xs))
      (groupBy (\x y -> takeBaseName (fst x) == takeBaseName (fst y)) r)
    takeBaseName path = intercalate "-" $ reverse $ drop 2 $ reverse $ splitOn "-" (last $ splitPath path)

writeTable :: (FilePath, [(String, Maybe Bool)]) -> IO ()
writeTable (path, t) = do
  time <- getCurrentTime
  writeFile (toConflictResultFolder path time) (mkTable t)
  where
    toConflictResultFolder path name =
      resultPath ++ path ++ "/" ++ show name ++ ".md"


mkTable :: [(String, Maybe Bool)] -> String
mkTable res = headers ++ "\n" ++ border ++ "\n" ++ values
  where
    headers = foldl addHeader "" res
    border  = foldl addBorder "" res
    values  = foldl addResults "" res

    addHeader :: String -> (String, Maybe Bool) -> String
    addHeader soFar (header,_)  =
      soFar ++ "| " ++ extract header ++ " | "

    addBorder :: String -> (String, Maybe Bool) -> String
    addBorder soFar x =
      soFar ++ "|" ++ replicate 13 '-' ++ "|"

    addResults :: String -> (String, Maybe Bool) -> String
    addResults soFar (_, result) =
      soFar ++ "|" ++ replicate 6 ' ' ++ convert result
        ++ replicate 6 ' ' ++ "|"
    convert r = case r of
      Just True  -> "T"
      Just False -> "F"
      Nothing    -> "X"

    extract :: FilePath -> String
    extract h = intercalate "-" $ map (take 5) (drop (length split - 2) split)
      where
        split = splitOn "-" (last (splitPath h))


runDiff :: IO Bool
runDiff = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in " ++ cwd
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

checkDisjointness :: IO (FilePath, Maybe Bool)
checkDisjointness = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in " ++ cwd
  ph <- spawnProcess executablePath ["-f", cwd ++ "/"]
  result <- timeout timeout_time (waitForProcess ph)
  case result of
    Nothing -> do
      terminateProcess ph
      putStrLn $ "Process Timed out"
      return (cwd, Nothing)
    Just (ExitFailure _) -> return $ (cwd, Just False)
    Just exitSuccess     -> return $ (cwd, Just True)

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
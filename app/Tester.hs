{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Process
import System.Directory
import System.Exit
import Control.Monad
import System.Timeout

import Clojure.AST
import Clojure.Parser
import Clojure.Lang

import VCS.Multirec
import VCS.Disjoint
import VCS.Cost
import VCS.Diff

import Oracle.Oracle
import Util.UnixDiff

testPath :: FilePath
testPath = "test/conflicts/mined"

executablePathRoot :: FilePath
executablePathRoot = ".stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/th-vcs-clojure-exe/th-vcs-clojure-exe"

executablePath = "../../../../" ++ executablePathRoot

timeout_time :: Int
timeout_time = 30000000

main :: IO ()
main = do
  dirs <- listDirectory testPath
  let dirsP = map (\d -> testPath ++ "/" ++ d) dirs
  actual_dirs <- filterM doesDirectoryExist dirsP
  results <- mapM (flip withCurrentDirectory checkDisjointness) actual_dirs
  let successes = filter (\b -> b == Just True) results
  let failures  = filter (\b -> b == Just False) results
  let timeouts = filter  (\b -> b == Nothing) results
  putStrLn $ "Succeded in " ++ show (length successes)
  putStrLn $ "\nFailed in " ++ show (length failures)
  putStrLn $ "\nWith " ++ show (length timeouts) ++  " timeouts "

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

checkDisjointness :: IO (Maybe Bool)
checkDisjointness = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in " ++ cwd
  ph <- spawnProcess executablePath []
  result <- timeout timeout_time (waitForProcess ph)
  case result of
    Nothing -> do
      terminateProcess ph
      putStrLn $ "Process Timed out"
      return Nothing
    Just (ExitFailure _) -> return $ Just False
    Just exitSuccess     -> return $ Just True

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
  let cp = buildCopyOracle (preprocess s d)
  let diff3 = buildDelInsMap $ preprocessGrouped s d
  putStrLn $ "cp" ++ show cp
  putStrLn $ "diff3" ++ show diff3
  return $ checkCopyMaps cp src dst

parseFile :: String -> String -> IO Expr
parseFile name src = case parse parseTop name src of
  Left err -> error $ show err
  Right s' -> return s'
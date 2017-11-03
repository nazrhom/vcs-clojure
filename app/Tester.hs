module Main where

import System.Process
import System.Directory
import Control.Monad
import System.Timeout

testPath :: FilePath
testPath = "test/conflicts/mined"

executablePathRoot :: FilePath
executablePathRoot = ".stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/th-vcs-clojure-exe/th-vcs-clojure-exe"

executablePath = "../../../../" ++ executablePathRoot

timeout_time :: Int
timeout_time = 60000000

main :: IO ()
main = do
  dirs <- listDirectory testPath
  let dirsP = map (\d -> testPath ++ "/" ++ d) dirs
  actual_dirs <- filterM doesDirectoryExist dirsP
  results <- mapM (flip withCurrentDirectory runDiff) actual_dirs
  let successes = filter (\b -> b == True) results
  putStrLn $ "Succeded in " ++ show (length successes) ++ " put of " ++ show (length results)

runDiff :: IO Bool
runDiff = do
  cwd <- getCurrentDirectory
  putStrLn $ "Running in " ++ cwd
  ph <- spawnProcess executablePath ["-s", "O1.clj", "-d", "A1.clj"]
  result <- timeout timeout_time (waitForProcess ph)
  case result of
    Nothing -> do
      terminateProcess ph
      putStrLn $ "Process Timed out"
      return False
    Just exitcode -> do
      putStrLn $ "Process terminated with exitcode " ++ show exitcode
      return True

module Main where

import Lib

import Data.EDN
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  f <- readFile "test/test.clj"
  let p = parse parseTop "" f
  putStrLn $ show p

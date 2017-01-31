module Main where

import Parser

import System.Environment

main :: IO ()
main = do
  (path:rest) <- getArgs
  f <- readFile path
  parseTest parseTop f

module Main where

import Parser
import PrettyPrint

import System.IO
import Options.Applicative
import Data.Monoid
import Diff
import Lang
import Apply
import RegularTypes

main :: IO ()
main = do
  let almus = diffAlmu (Ui sum1) (Ui sum2)
  let patches = map (flip applyAlmu (Ui sum1)) almus
  putStrLn $ "All the same? " ++ show (allTheSame patches)
  putStrLn $ show $ applyAlmu (head almus) (Ui sum1)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

a = Value (1)
b = Value (1)
c = Value (2)
d = Value (2)
sum1 = Add a b
sum2 = Add c d
square1 = Square a
sum3 = Add sum1 sum2
sum4 = Add sum2 square1

almus :: [Almu]
almus = diffAlmu (Ui sum1) (Ui sum2)

{-# LANGUAGE FlexibleInstances #-}

module Util.UnixDiff where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput hiding (LineRange)

import Clojure.AST

preprocess :: String -> String -> [DiffResult]
preprocess f1 f2 = map processDiff diff
  where
    diff = getDiffBy ignoringLines (withLineN f1) (withLineN f2)


data DiffResult = DiffResult DiffAction Int
data DiffAction = Mod | Ins | Del
  deriving Eq

withLineN :: String -> [(String, Int)]
withLineN s = zip (lines s) [1..]

ignoringLines s1 s2 = fst s1 == fst s2

ppPDiff :: [DiffResult] -> String
ppPDiff = foldl (\d a -> d ++ "\n" ++ show a) ""

processDiff :: Diff (String, Int) -> DiffResult
processDiff (Both (s, i) _) = DiffResult Mod i
processDiff (First (s, i))  = DiffResult Ins i
processDiff (Second (s, i)) = DiffResult Del i

instance Show DiffResult where
  show (DiffResult a i) = show i ++ ": " ++ show a

instance Show DiffAction where
  show Mod = "%"
  show Ins = "+"
  show Del = "-"

{-# LANGUAGE FlexibleInstances #-}

module Util.UnixDiff where

import Data.Algorithm.Diff
import qualified Data.Algorithm.DiffOutput as O

import Clojure.AST

data GroupDiffAction = OMod LineRange LineRange
                     | OIns LineRange Int
                     | ODel LineRange Int
  deriving (Show)

data DiffAction = Copy (Int, Int)
                | Ins Int
                | Del Int
  deriving Eq

preprocess :: String -> String -> [DiffAction]
preprocess s1 s2 = map processDiff (diff s1 s2)

preprocessGrouped :: String -> String -> [GroupDiffAction]
preprocessGrouped s1 s2 = map processGroupedDiff (groupedDiff s1 s2)

diff :: String -> String -> [Diff (String, Int)]
diff s1 s2 = getDiffBy eqIgnoringLines (withLineN s1) (withLineN s2)

groupedDiff :: String -> String -> [O.DiffOperation O.LineRange]
groupedDiff f1 f2 = O.diffToLineRanges $ getGroupedDiff (lines f1) (lines f2)

withLineN :: String -> [(String, Int)]
withLineN s = zip (lines s) [1..]

eqIgnoringLines s1 s2 = fst s1 == fst s2

ppPDiff :: [DiffAction] -> String
ppPDiff = foldl (\d a -> d ++ "\n" ++ show a) ""

processDiff :: Diff (String, Int) -> DiffAction
processDiff (Both (_, i1) (_, i2)) = (Copy (i1, i2))
processDiff (First (_, i))  = (Del i)
processDiff (Second (_, i)) = (Ins i)

processGroupedDiff :: O.DiffOperation O.LineRange -> GroupDiffAction
processGroupedDiff (O.Change srcR dstR) = OMod (extractLineRange srcR) (extractLineRange dstR)
processGroupedDiff (O.Addition lr line) = OIns (extractLineRange lr) line
processGroupedDiff (O.Deletion lr line) = ODel (extractLineRange lr) line

extractLineRange :: O.LineRange -> LineRange
extractLineRange lr = Range start end
  where
    (start, end) = O.lrNumbers lr

instance Show DiffAction where
  show (Copy (i1, i2)) = show i1 ++ " % " ++ show i2
  show (Ins i) = show i ++ "+"
  show (Del i) = show i ++ "-"

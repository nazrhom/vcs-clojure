module Main where

import Parser
import PrettyPrint

import System.IO
import Options.Applicative
import Data.Monoid
import Diff
import Lang
import Apply
import Multirec

main :: IO ()
main = mapM_ checkPair $ zip almus patches

checkPair (alums, patches) = do
  putStrLn $ "All the same? " ++ show (allTheSame patches)
  putStrLn $ show $ head patches

almus1 = diffAlmu M (Usexpr op1) (UsexprL list1)
almus2 = diffAlmu M (Usexpr oplist1) (UsexprL listop2)
almus3 = diffAlmu M (Usexpr oplist2) (UsexprL listop2)
almus4 = diffAlmu M (Usexpr oplist3) (UsexprL listop3)
almus5 = diffAlmu M (Usexpr oplist1) (UsexprL list1)
almus6 = diffAlmu M (Usexpr oplist2) (UsexprL listop3)

patches1 = map (flip applyAlmu (Usexpr op1)) almus1
patches2 = map (flip applyAlmu (Usexpr oplist1)) almus2
patches3 = map (flip applyAlmu (Usexpr oplist2)) almus3
patches4 = map (flip applyAlmu (Usexpr oplist3)) almus4
patches5 = map (flip applyAlmu (Usexpr oplist1)) almus5
patches6 = map (flip applyAlmu (Usexpr oplist2)) almus6

almus = [almus1, almus2, almus3, almus4, almus5, almus6]
patches = [patches1, patches2, patches3, patches4, patches5, patches6]

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

list1 = SCons testval1 (SCons testval2 SNil)
list2 = SCons testval2 (SCons testval1 SNil)
testval1 = Value 0
testval2 = Value 1
op1 = Operation testval1 testval2
op2 = Operation testval2 testval1

oplist1 = Operation (List list1) (List list1)
listop1 = SCons op1 (SCons op1 SNil)

oplist2 = Operation (List list1) (List list2)
listop2 = SCons op1 (SCons op2 SNil)

oplist3 = Operation (List listop2) (List listop1)
listop3 = SCons oplist1 (SCons oplist2 SNil)

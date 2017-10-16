{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Oracle.GroupedDiffOracle where

import Data.Maybe

import Oracle.Internal
import Clojure.Lang
import Clojure.AST
import Util.UnixDiff

data GroupedDiffOracle = GroupedDiffOracle [GroupDiffAction]

askOracleGrp :: GroupedDiffOracle -> LineRange -> LineRange -> [Path]
askOracleGrp (GroupedDiffOracle o) srcRange dstRange =
  giveAdvice o srcRange dstRange

giveAdvice :: [GroupDiffAction] -> LineRange -> LineRange -> [Path]
giveAdvice [] _ _ = [ M ]
giveAdvice ((OMod src dst):os) srcRange dstRange =
  if srcRange `isContainedIn` src && dstRange `isContainedIn` dst then []
  else giveAdvice os srcRange dstRange
giveAdvice ((OIns lr i):os) (Range s e) dstRange =
  if dstRange `isContainedIn` lr && s == i
  then [ I ]
  else giveAdvice os (Range s e) dstRange
giveAdvice ((ODel lr i):os) srcRange (Range s e) =
  if srcRange `isContainedIn` lr && s == i
  then [ D ]
  else giveAdvice os srcRange (Range s e)

isContainedIn :: LineRange -> LineRange -> Bool
isContainedIn (Range s1 e1) (Range s2 e2) = s2 >= s1 && e1 <= e2

instance (Monad m) => OracleF GroupedDiffOracle m where
  callF o s d = do
    return $ askOracleGrp o (fromJust $ extractRange s) (fromJust $ extractRange d)

instance (Monad m) => OracleP GroupedDiffOracle m where
  callP _ An         An         = return []
  callP _ An         (_ `Ac` _) = return [ I ]
  callP _ (_ `Ac` _) An         = return [ D ]
  callP o (s `Ac` _) (d `Ac` _) = case (extractRange s, extractRange d) of
    (Nothing, Nothing)         -> return [ M ]
    (Just sRange, Nothing)     -> return [ D ]
    (Nothing, Just dRange)     -> return [ I ]
    (Just sRange, Just dRange) -> return $ askOracleGrp o sRange dRange

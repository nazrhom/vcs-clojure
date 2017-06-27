module Util.DiffParser where

import Control.Monad.State
import Data.Char hiding (Space)
import Text.Parsec

import Clojure.PrettyPrint

data FileTarget = FileTarget
  { mine :: String
  , yours :: String
  , parent :: String
  , current :: [String]
  }

defaultTarget :: String -> String -> String -> FileTarget
defaultTarget m p y = FileTarget
  { mine = m
  , yours = y
  , parent = p
  , current = [m, y, p]
  }

data Phase = Mine | Yours | Parent | Reset
type MyState = StateT FileTarget IO

mineStart   = Mine <$ string "<<<<<<< A.clj"
parentStart = Parent <$ string "||||||| O.clj"
yoursStart  = Yours <$ string "======="
yoursEnd    = Reset <$ string ">>>>>>> B.clj"

process :: String -> String -> String -> String -> IO ()
process m p y src = do
  let initialState = defaultTarget m p y
  evalStateT (handleFile src) initialState

handleFile :: String -> MyState ()
handleFile i = do
  let els = lines i
  mapM_ handleLine els

handleLine :: String -> MyState ()
handleLine l = do
  p <- getTarget l
  case p of
    Left e -> writeLine l
    Right t -> do
      setTarget t
      return ()

getTarget :: String -> MyState (Either ParseError Phase)
getTarget l = runParserT checkModifiers () "" l

setTarget :: Phase -> MyState ()
setTarget Mine = setMine
setTarget Yours = setYours
setTarget Parent = setParent
setTarget Reset = resetState

checkModifiers :: ParsecT String () MyState Phase
checkModifiers = choice
  [ try mineStart
  , try yoursStart
  , try parentStart
  , try yoursEnd ]

writeLine :: String -> MyState ()
writeLine l = do
  s <- get
  liftIO $ mapM_ (flip appendFile (l ++ "\n")) (current s)

setMine :: MyState ()
setMine = do
  s <- get
  put (s {current = [mine s] })

setYours :: MyState ()
setYours = do
  s <- get
  put (s {current = [yours s] })

setParent :: MyState ()
setParent = do
  s <- get
  put (s {current = [parent s] })

resetState :: MyState ()
resetState = do
  s <- get
  put $ defaultTarget (mine s) (yours s) (parent s)

minimize :: Eq a => [a] -> [a] -> ([a], [a])
minimize [] [] = ([], [])
minimize (a:as) (b:bs) = if a == b then (remA, remB) else (a:remA , b:remB)
  where
    (remA, remB) = minimize as bs

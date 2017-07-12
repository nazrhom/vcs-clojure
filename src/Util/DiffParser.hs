module Util.DiffParser where

import Control.Monad.State
import Control.Monad
import Data.Char hiding (Space)
import Text.Parsec
import System.Directory
import System.IO

import Clojure.PrettyPrint
import Clojure.Parser

testPath = "test/conflicts/mined"

runMinify :: IO ()
runMinify = do
  dirs <- listDirectory testPath
  let dirsP = map (\d -> testPath ++ "/" ++ d) dirs
  actual_dirs <- filterM doesDirectoryExist dirsP
  mapM_ (flip withCurrentDirectory processDir) actual_dirs

processDir :: IO ()
processDir = do
  merge <- readFile "M.clj"
  process "A1.clj" "O1.clj" "B1.clj" merge
  files <- readFiles
  (a1, o1, b1) <- parseFiles files
  let (a, o, b) = minimize a1 o1 b1

  writeFile "A1.clj" $ ppLines a
  writeFile "O1.clj" $ ppLines o
  writeFile "B1.clj" $ ppLines b

readFiles :: IO (String, String, String)
readFiles = do
  a1 <- readFile "A1.clj"
  o1 <- readFile "O1.clj"
  b1 <- readFile "B1.clj"
  return (a1, o1, b1)

parseFiles :: (String, String, String) -> IO ([Expr], [Expr], [Expr])
parseFiles (a, o, b) = do
  a1 <- parseFile a
  o1 <- parseFile o
  b1 <- parseFile b
  return (a1, o1, b1)

parseFile :: String -> IO [Expr]
parseFile a = do
  cwd <- getCurrentDirectory
  case parse parseAsExprList cwd a of
    Left err -> error $ show err
    Right f  -> return f

data FileTarget = FileTarget
  { mine :: Handle
  , yours :: Handle
  , parent :: Handle
  , current :: [Handle]
  }

defaultTarget :: Handle -> Handle -> Handle -> FileTarget
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

process :: FilePath -> FilePath -> FilePath -> String -> IO ()
process m p y src = do
  m' <- openFile m WriteMode
  p' <- openFile p WriteMode
  y' <- openFile y WriteMode
  let initialState = defaultTarget m' p' y'
  evalStateT (handleFile src) initialState
  hClose m'
  hClose p'
  hClose y'

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
  liftIO $ mapM_ (flip hPutStrLn l) (current s)

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

minimize :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
minimize [] (o:os) (b:bs) = if o == b then ([], remO, remB)
  else ([], o:remO, b:remB)
  where
    (remA, remO, remB) = minimize [] os bs
minimize (a:as) (o:os) [] = if o == a then (remA, remO, [])
  else (a:remA, o:remO, [])
  where
    (remA, remO, remB) = minimize as os []
minimize (a:as) (o:os) (b:bs) =
  if a == o && o == b
  then (remA, remO, remB)
  else if a == o then
      (remANoB, o:remONoB, b:remBNoB)
  else if b == o then
    (a:remANoB, o:remONoB, remBNoB)
  else (a:remA, o:remO, b:remB)
  where
    (remA, remO, remB) = minimize as os bs
    (remANoB, remONoB, remBNoB) = minimize as os (b:bs)
    (remANoA, remONoA, remBNoA) = minimize (a:as) os bs
minimize a o b = (a, o, b)

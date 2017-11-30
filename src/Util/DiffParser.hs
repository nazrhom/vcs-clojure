module Util.DiffParser where

import Control.Monad.State
import Control.Monad
import Data.Char hiding (Space)
import Text.Parsec
import System.Directory
import System.IO
import Data.List

import Language.Clojure.PrettyPrint
import Language.Clojure.Parser
import Language.Clojure.AST

import Debug.Trace

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
  (fA, fO, fB) <- readFiles
  (pA, pO, pB) <- parseFiles (fA, fO, fB)
  let (a, o, b) = minimize pA pO pB
      lrA = map extractRangeExpr a
      lrO = map extractRangeExpr o
      lrB = map extractRangeExpr b

  writeSelectedLines fA "A1.clj" lrA
  writeSelectedLines fO "O1.clj" lrO
  writeSelectedLines fB "B1.clj" lrB

writeSelectedLines :: String -> FilePath -> [LineRange] -> IO ()
writeSelectedLines f tgt lrs = do
  let fl = lines f
  writeFile tgt (unlines $ concat $ extractLines fl lrs)
    where
      extractLines _ [] = []
      extractLines l ((Range start end):rest) =
        (extractRange start end l):(extractLines l rest)

      extractRange s e l | s < e = (l !! (s - 1)):(extractRange (s+1) e l)
      extractRange s e l | otherwise = []



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
minimize as os bs =
  (as \\ asCommonElems, os \\ osCommonElemns, bs \\ bsCommonElemns)
  where
    asCommonElems = [a | a <- as,  a `elem` os, a `elem` bs]
    osCommonElemns = [o | o <- os, o `elem` as, o `elem` bs]
    bsCommonElemns = [b | b <- bs, b `elem` as, b `elem` os]

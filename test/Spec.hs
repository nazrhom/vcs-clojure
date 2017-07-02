import Clojure.Parser
import Clojure.PrettyPrint
import System.FilePath.Find
import System.Directory

main :: IO ()
main = do
  cljFiles <- find always (extension ==? ".clj") "./test/repos"
  mapM_ testFile cljFiles

parseClj :: String -> Expr
parseClj f = case parse parseTop "test" f of
  Left err -> error $ show err
  Right expr -> expr

parseReparse :: String -> Bool
parseReparse x = (parseClj . ppTop . parseClj) x == parseClj x

testFile :: FilePath -> IO ()
testFile f = do
  putStrLn $ "\nTesting file " ++ show f
  contents <- readFile f
  case parse parseTop f contents of
    Left err -> error $ "Error on first parse " ++  show err
    Right p -> do
      case parse parseTop ("Parse Result of: " ++ f) (ppTop p) of
        Left err -> error $ "Error on second parse " ++ show err
        Right p1 -> case p == p1 of
          True -> return ()
          False -> error $ "Parse != Reparse\n" ++ show p ++ "\n" ++ show p1

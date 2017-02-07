import Parser
import PrettyPrint
import System.FilePath.Posix
import System.Directory

main :: IO ()
main = do
  cljFiles <- getDirectoryContents "./test" >>= return . map makePath . filter isCljFile
  mapM_ testFile cljFiles
  where
    makePath = (</>) "./test"
isCljFile :: FilePath -> Bool
isCljFile f = takeExtension f == ".clj"

parseClj :: String -> [Expr]
parseClj f = case parse parseTop "test" f of
  Left err -> error $ show err
  Right expr -> expr

parseReparse :: String -> Bool
parseReparse x = (parseClj . ppTop . parseClj) x == parseClj x

testFile :: FilePath -> IO ()
testFile f = do
  putStrLn $ "\nTesting file " ++ show f
  contents <- readFile f
  putStrLn $ "Parse == Reparse " ++ show (parseReparse contents)

import Curry.LanguageServer.Diagnostics
import qualified Language.Haskell.LSP.Types as J
import Files.CymakePath

main :: IO ()
main = do
    curryPath <- getCymake
    putStrLn $ "Curry-Frontend path: " ++ curryPath
    diags <- fetchDiagnostics uri ["curry-imports"]
    putStrLn $ "Diagnostics: " ++ show (head diags)
    where uri = J.filePathToUri "test/resources/Test.curry"

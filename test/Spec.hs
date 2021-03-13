import Curry.LanguageServer.Diagnostics
import qualified Language.LSP.Types as J
import Files.CymakePath

main :: IO ()
main = do
    curryPath <- getCymake
    putStrLn $ "Curry-Frontend path: " ++ curryPath
    diags <- fetchDiagnostics ["curry-libs"] uri
    putStrLn $ "Diagnostics: " ++ show (head diags)
    where uri = J.filePathToUri "test/resources/Test.curry"

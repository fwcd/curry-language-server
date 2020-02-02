import Curry.LanguageServer.Diagnostics
import qualified Language.Haskell.LSP.Types as J

main :: IO ()
main = do
    diags <- fetchDiagnostics uri
    putStrLn $ "Diagnostics: " ++ show (head diags)
    where uri = J.toNormalizedUri $ J.filePathToUri "test/resources/Test.curry"

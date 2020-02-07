module Curry.LanguageServer.Compiler (
    CompilationOutput (..),
    CompilationResult,
    compileCurry,
    parseInterface,
    compilationToMaybe,
    failedCompilation
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Files.PathUtils as CF
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import Curry.Base.Monad (CYIO, runCYIO, runCYM, failMessages)
import qualified Curry.Base.Position as CP
import qualified Curry.Syntax as CS
import qualified CurryDeps as CD
import qualified CompilerEnv as CE
import qualified CompilerOpts as CO
import Modules (loadAndCheckModule)
import qualified Text.PrettyPrint as PP

import Control.Monad.Reader
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import qualified Data.Map as M
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Language.Haskell.LSP.Types as J
import System.Directory
import System.FilePath

data CompilationOutput = CompilationOutput { compilerEnv :: CE.CompilerEnv, moduleASTs :: [(FilePath, ModuleAST)] }
type CompilationResult = Either [CM.Message] (CompilationOutput, [CM.Message])

-- | Compiles a Curry source file with it's dependencies
-- using the given import paths. If compilation fails the
-- result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurry :: [FilePath] -> FilePath -> IO CompilationResult
compileCurry importPaths filePath = runCYIO $ do
    deps <- ((maybeToList . depToFilePath) =<<) <$> CD.flatDeps opts filePath
    liftIO $ logs INFO $ "Compiling Curry, found deps: " ++ show (takeFileName <$> snd <$> deps)
    toCompilationOutput <$> loadAndCheckModules opts deps
    where cppOpts = CO.optCppOpts CO.defaultOptions
          cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
          opts = CO.defaultOptions { CO.optForce = True,
                                     CO.optImportPaths = importPaths,
                                     CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }

-- | Compiles the given list of modules in order.
loadAndCheckModules :: CO.Options -> [(CI.ModuleIdent, FilePath)] -> CYIO (CE.CompEnv [(FilePath, ModuleAST)])
loadAndCheckModules opts deps = case deps of
    [] -> failMessages [failMessageFrom "Language Server: No module found"]
    [(m, fp)] -> do
        liftIO $ logs INFO $ "Loading module " ++ takeFileName fp
        (\ast -> [(fp, ast)]) <$.> loadAndCheckModule opts m fp
    ((m, fp):ds) -> do
        liftIO $ logs INFO $ "Loading modules " ++ takeFileName fp ++ ", ..."
        (_, ast) <- loadAndCheckModule opts m fp
        (env, asts) <- loadAndCheckModules opts ds
        return (env, ((fp, ast) : asts))

parseInterface :: FilePath -> IO (Maybe CS.Interface)
parseInterface fp = do
    logs DEBUG $ "parseInterface: Parsing interface at " ++ fp
    src <- CF.readModule fp
    return $ (eitherToMaybe . (fst <$>) . runCYM . CS.parseInterface fp) =<< src

compilationToMaybe :: CompilationResult -> Maybe CompilationOutput
compilationToMaybe = (fst <$>) . eitherToMaybe

depToFilePath :: (CI.ModuleIdent, CD.Source) -> Maybe (CI.ModuleIdent, FilePath)
depToFilePath (m, (CD.Source fp _ _)) = Just (m, fp)
depToFilePath _ = Nothing

depMatches :: FilePath -> CD.Source -> Bool
depMatches fp1 (CD.Source fp2 _ _) = fp1 == fp2
depMatches _ _ = False

toCompilationOutput :: CE.CompEnv [(FilePath, ModuleAST)] -> CompilationOutput
toCompilationOutput (env, asts) = CompilationOutput { compilerEnv = env, moduleASTs = asts }

justOrFail :: String -> Maybe a -> CYIO a
justOrFail _ (Just x) = return x
justOrFail msg Nothing = failMessages [failMessageFrom msg]

failedCompilation :: String -> CompilationResult
failedCompilation msg = Left [failMessageFrom msg]

failMessageFrom :: String -> CM.Message
failMessageFrom = CM.message . PP.text

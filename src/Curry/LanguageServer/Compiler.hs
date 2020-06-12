module Curry.LanguageServer.Compiler (
    CompilationOutput (..),
    CompilationResult,
    compileCurryFileWithDeps,
    compilationToMaybe,
    failedCompilation
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Files.Filenames as CFN
import qualified Curry.Files.PathUtils as CF
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import Curry.Base.Monad (CYIO, runCYIO, runCYM, failMessages)
import qualified Curry.Base.Position as CP
import qualified Curry.Syntax as CS
import qualified Checks as CC
import qualified CurryDeps as CD
import qualified CompilerEnv as CE
import qualified CompilerOpts as CO
import qualified Exports as CEX
import Modules (loadAndCheckModule)
import qualified Transformations as CT
import qualified Text.PrettyPrint as PP

import Control.Monad.Reader
import qualified Curry.LanguageServer.Config as CFG
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
-- using the given import paths and the given output directory
-- (in which the interface file will be placed). If compilation fails the
-- result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurryFileWithDeps :: CFG.Config -> [FilePath] -> FilePath -> FilePath -> IO CompilationResult
compileCurryFileWithDeps cfg importPaths outDirPath filePath = runCYIO $ do
    let cppOpts = CO.optCppOpts CO.defaultOptions
        cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
        opts = CO.defaultOptions { CO.optForce = CFG.forceRecompilation cfg,
                                   CO.optImportPaths = importPaths ++ CFG.importPaths cfg,
                                   CO.optLibraryPaths = CFG.libraryPaths cfg,
                                   CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }
    -- Resolve dependencies
    deps <- ((maybeToList . expandDep) =<<) <$> CD.flatDeps opts filePath
    liftIO $ logs DEBUG $ "Compiling Curry, found deps: " ++ show (takeFileName <$> snd3 <$> deps)
    -- Process pragmas
    let opts' = foldl processPragmas opts $ thd3 <$> deps
    -- Compile the module and its dependencies in topological order
    toCompilationOutput <$> (compileCurryModules opts' outDirPath $ tripleToPair <$> deps)
    where processPragmas :: CO.Options -> [CS.ModulePragma] -> CO.Options
          processPragmas o ps = foldl processExtensionPragma o [e | CS.LanguagePragma _ es <- ps, CS.KnownExtension _ e <- es]
          processExtensionPragma :: CO.Options -> CS.KnownExtension -> CO.Options
          processExtensionPragma o e = case e of
              CS.CPP -> o { CO.optCppOpts = (CO.optCppOpts o) { CO.cppRun = True } }
              _      -> o

-- | Compiles the given list of modules in order.
compileCurryModules :: CO.Options -> FilePath -> [(CI.ModuleIdent, FilePath)] -> CYIO (CE.CompEnv [(FilePath, ModuleAST)])
compileCurryModules opts outDirPath deps = case deps of
    [] -> failMessages [failMessageFrom "Language Server: No module found"]
    [(m, fp)] -> (\ast -> [(fp, ast)]) <$.> compileCurryModule opts outDirPath m fp
    ((m, fp):ds) -> do
        (_, ast) <- compileCurryModule opts outDirPath m fp
        (env, asts) <- compileCurryModules opts outDirPath ds
        return (env, ((fp, ast) : asts))

-- | Compiles a single module.
compileCurryModule :: CO.Options -> FilePath -> CI.ModuleIdent -> FilePath -> CYIO (CE.CompEnv ModuleAST)
compileCurryModule opts outDirPath m fp = do
    liftIO $ logs INFO $ "Compiling module " ++ takeFileName fp
    -- Parse and check the module
    mdl <- loadAndCheckModule opts m fp
    -- Generate and store an on-disk interface file
    mdl' <- CC.expandExports opts mdl
    let interf = uncurry CEX.exportInterface $ CT.qual mdl'
        interfFilePath = outDirPath </> (CFN.interfName $ CFN.moduleNameToFile m)
        generated = PP.render $ CS.pPrint interf
    liftIO $ logs DEBUG $ "Writing interface file to " ++ interfFilePath
    liftIO $ CF.writeModule interfFilePath generated 
    return mdl

compilationToMaybe :: CompilationResult -> Maybe CompilationOutput
compilationToMaybe = (fst <$>) . eitherToMaybe

expandDep :: (CI.ModuleIdent, CD.Source) -> Maybe (CI.ModuleIdent, FilePath, [CS.ModulePragma])
expandDep (m, (CD.Source fp prags _)) = Just (m, fp, prags)
expandDep _ = Nothing

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

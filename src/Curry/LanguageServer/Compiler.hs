module Curry.LanguageServer.Compiler (
    CompilationOutput (..),
    CompilationResult,
    FileLoader,
    compileCurryFileWithDeps,
    compilationToMaybe,
    failedCompilation
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Files.Filenames as CFN
import qualified Curry.Files.PathUtils as CF
import qualified Curry.Files.Unlit as CUL
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Span as CSP
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Base.Message as CM
import Curry.Base.Monad (CYIO, CYT, runCYIO, runCYM, liftCYM, silent, failMessages, warnMessages)
import qualified Curry.Base.Position as CP
import qualified Curry.Syntax as CS
import qualified Base.Messages as CBM
import qualified Checks as CC
import qualified CurryDeps as CD
import qualified CompilerEnv as CE
import qualified CondCompile as CNC
import qualified CompilerOpts as CO
import qualified Env.Interface as CEI
import qualified Exports as CEX
import qualified Imports as CIM
import qualified Interfaces as CIF
import qualified Modules as CMD
import qualified Transformations as CT
import qualified Text.PrettyPrint as PP

import Control.Monad (liftM)
import Control.Monad.Reader
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Syntax (ModuleAST)
import qualified Data.Map as M
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (maybeToList)
import System.FilePath

data CompilationOutput = CompilationOutput { compilerEnv :: CE.CompilerEnv, moduleASTs :: [(FilePath, ModuleAST)] }
type CompilationResult = Either [CM.Message] (CompilationOutput, [CM.Message])
type FileLoader = FilePath -> IO String

-- | Compiles a Curry source file with its dependencies
-- using the given import paths and the given output directory
-- (in which the interface file will be placed). If compilation fails the
-- result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurryFileWithDeps :: CFG.Config -> FileLoader -> [FilePath] -> FilePath -> FilePath -> IO CompilationResult
compileCurryFileWithDeps cfg fl importPaths outDirPath filePath = runCYIO $ do
    let cppOpts = CO.optCppOpts CO.defaultOptions
        cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
        opts = CO.defaultOptions { CO.optForce = CFG.forceRecompilation cfg,
                                   CO.optImportPaths = importPaths ++ CFG.importPaths cfg,
                                   CO.optLibraryPaths = CFG.libraryPaths cfg,
                                   CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }
    -- Resolve dependencies
    deps <- ((maybeToList . expandDep) =<<) <$> CD.flatDeps opts filePath
    liftIO $ logs DEBUG $ "compiler: Compiling Curry, found deps: " ++ show (takeFileName . snd3 <$> deps)
    -- Process pragmas
    let opts' = foldl processPragmas opts $ thd3 <$> deps
    -- Compile the module and its dependencies in topological order
    toCompilationOutput <$> compileCurryModules opts' fl outDirPath (tripleToPair <$> deps)
    where processPragmas :: CO.Options -> [CS.ModulePragma] -> CO.Options
          processPragmas o ps = foldl processExtensionPragma o [e | CS.LanguagePragma _ es <- ps, CS.KnownExtension _ e <- es]
          processExtensionPragma :: CO.Options -> CS.KnownExtension -> CO.Options
          processExtensionPragma o e = case e of
              CS.CPP -> o { CO.optCppOpts = (CO.optCppOpts o) { CO.cppRun = True } }
              _      -> o

-- | Compiles the given list of modules in order.
compileCurryModules :: CO.Options -> FileLoader -> FilePath -> [(CI.ModuleIdent, FilePath)] -> CYIO (CE.CompEnv [(FilePath, ModuleAST)])
compileCurryModules opts fl outDirPath deps = case deps of
    [] -> failMessages [failMessageFrom "Language Server: No module found"]
    [(m, fp)] -> (\ast -> [(fp, ast)]) <$.> compileCurryModule opts fl outDirPath m fp
    ((m, fp):ds) -> do
        (_, ast) <- compileCurryModule opts fl outDirPath m fp
        (env, asts) <- compileCurryModules opts fl outDirPath ds
        return (env, (fp, ast) : asts)

-- | Compiles a single module.
compileCurryModule :: CO.Options -> FileLoader -> FilePath -> CI.ModuleIdent -> FilePath -> CYIO (CE.CompEnv ModuleAST)
compileCurryModule opts fl outDirPath m fp = do
    liftIO $ logs DEBUG $ "compiler: Compiling module " ++ takeFileName fp
    -- Parse and check the module
    mdl <- loadAndCheckCurryModule opts fl m fp
    -- Generate and store an on-disk interface file
    mdl' <- CC.expandExports opts mdl
    let interf = uncurry CEX.exportInterface $ CT.qual mdl'
        interfFilePath = outDirPath </> CFN.interfName (CFN.moduleNameToFile m)
        generated = PP.render $ CS.pPrint interf
    liftIO $ logs DEBUG $ "compiler: Writing interface file to " ++ interfFilePath
    liftIO $ CF.writeModule interfFilePath generated 
    return mdl

-- The following functions partially reimplement
-- https://git.ps.informatik.uni-kiel.de/curry/curry-frontend/-/blob/master/src/Modules.hs
-- since the original module loader/parser does not support virtualized file systems.
-- License     :  BSD-3-clause
-- Copyright   :  (c) 1999 - 2004 Wolfgang Lux
--                    2005        Martin Engelke
--                    2007        Sebastian Fischer
--                    2011 - 2015 Björn Peemöller
--                    2016        Jan Tikovsky
--                    2016 - 2017 Finn Teegen
--                    2018        Kai-Oliver Prott

-- | Loads a single module and performs checks.
loadAndCheckCurryModule :: CO.Options -> FileLoader -> CI.ModuleIdent -> FilePath -> CYIO (CE.CompEnv ModuleAST)
loadAndCheckCurryModule opts fl m fp = do
    -- Read source file (possibly from VFS)
    src <- liftIO $ fl fp
    -- Load and check module
    ce <- CMD.checkModule opts =<< loadCurryModule opts m src fp
    warnMessages $ uncurry (CC.warnCheck opts) ce
    return ce

-- | Loads a single module.
loadCurryModule :: CO.Options -> CI.ModuleIdent -> String -> FilePath -> CYIO (CE.CompEnv (CS.Module()))
loadCurryModule opts m src fp = do
    -- Parse the module
    (lex, ast) <- parseCurryModule opts m src fp
    -- Load the imported interfaces into an InterfaceEnv
    let paths = (CFN.addCurrySubdir $ CO.optUseSubdir opts) <$> ("." : CO.optImportPaths opts)
    let withPrelude = importCurryPrelude opts ast
    iEnv <- CIF.loadInterfaces paths withPrelude
    checkInterfaces opts iEnv
    is <- importSyntaxCheck iEnv withPrelude
    -- Add Information of imported modules
    cEnv <- CIM.importModules withPrelude iEnv is
    return (cEnv { CE.filePath = fp, CE.tokens = lex }, ast)

-- | Checks all interfaces.
checkInterfaces :: Monad m => CO.Options -> CEI.InterfaceEnv -> CYT m ()
checkInterfaces opts iEnv = mapM_ checkInterface $ M.elems iEnv
    where checkInterface intf = do
            let env = CIM.importInterfaces intf iEnv
            CC.interfaceCheck opts (env, intf)

-- | Checks all imports in the module.
importSyntaxCheck :: Monad m => CEI.InterfaceEnv -> CS.Module a -> CYT m [CS.ImportDecl]
importSyntaxCheck iEnv (CS.Module _ _ _ _ _ is _) = mapM checkImportDecl is
    where checkImportDecl (CS.ImportDecl p m q asM is) = case M.lookup m iEnv of
            Just intf -> CS.ImportDecl p m q asM `liftM` CC.importCheck intf is
            Nothing   -> CBM.internalError $ "compiler: No interface for " ++ show m

-- | Ensures that a Prelude is present in the module.
importCurryPrelude :: CO.Options -> CS.Module () -> CS.Module ()
importCurryPrelude opts m@(CS.Module spi li ps mid es is ds) | needed    = CS.Module spi li ps mid es (preludeImpl : is) ds
                                                             | otherwise = m
    where isPrelude = mid == CI.preludeMIdent
          disabled = CS.NoImplicitPrelude `elem` CO.optExtensions opts || m `CS.hasLanguageExtension` CS.NoImplicitPrelude
          imported = CI.preludeMIdent `elem` ((\(CS.ImportDecl _ i _ _ _) -> i) <$> is)
          needed = not isPrelude && not disabled && not imported
          preludeImpl = CS.ImportDecl CSPI.NoSpanInfo CI.preludeMIdent False Nothing Nothing

-- | Parses a single module.
parseCurryModule :: CO.Options -> CI.ModuleIdent -> String -> FilePath -> CYIO ([(CSP.Span, CS.Token)], CS.Module ())
parseCurryModule opts m src fp = do
    ul <- liftCYM $ CUL.unlit fp src
    -- TODO: Preprocess
    cc <- CNC.condCompile (CO.optCppOpts opts) fp ul
    lex <- liftCYM $ silent $ CS.lexSource fp cc
    ast <- liftCYM $ CS.parseModule fp cc
    -- TODO: Check module/file mismatch?
    return (lex, ast)

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

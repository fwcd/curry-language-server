{-# LANGUAGE LambdaCase, NoFieldSelectors, OverloadedStrings, OverloadedRecordDot, FlexibleContexts #-}
module Curry.LanguageServer.Compiler
    ( CompileAuxiliary (..)
    , CompileState (..)
    , CompileOutput
    , FileLoader
    , compileCurryFileWithDeps
    , failedCompilation
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Files.Filenames as CFN
import qualified Curry.Files.PathUtils as CF
import qualified Curry.Files.Unlit as CUL
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Span as CSP
import qualified Curry.Base.SpanInfo as CSPI
import qualified Curry.Base.Message as CM
import Curry.Base.Monad (CYIO, CYT, runCYIO, liftCYM, silent, failMessages, warnMessages)
import qualified Curry.Syntax as CS
import qualified Curry.Syntax.Extension as CSE
import qualified Curry.Frontend.Base.Messages as CBM
import qualified Curry.Frontend.Checks as CC
import qualified Curry.Frontend.CurryBuilder as CB
import qualified Curry.Frontend.CurryDeps as CD
import qualified Curry.Frontend.CompilerEnv as CE
import qualified Curry.Frontend.CondCompile as CNC
import qualified Curry.Frontend.CompilerOpts as CO
import qualified Curry.Frontend.Env.Interface as CEI
import qualified Curry.Frontend.Exports as CEX
import qualified Curry.Frontend.Imports as CIM
import qualified Curry.Frontend.Interfaces as CIF
import qualified Curry.Frontend.Modules as CMD
import qualified Curry.Frontend.Transformations as CT
import qualified Text.PrettyPrint as PP

import Control.Monad (join, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (modify, gets)
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Utils.Convert (ppToText)
import Curry.LanguageServer.Utils.General ((<.$>))
import Curry.LanguageServer.Utils.Logging (debugM)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.LSP.Server (MonadLsp)
import System.FilePath ((</>), takeFileName)

type FileLoader = FilePath -> IO String

-- | Read-only state used during compilation.
newtype CompileAuxiliary = CompileAuxiliary
    { fileLoader :: FileLoader
    }

-- | Read/write state used during compilation.
data CompileState = CompileState
    { warnings :: [CM.Message]
    , errors :: [CM.Message]
    }

instance Semigroup CompileState where
    x <> y = CompileState
        { warnings = x.warnings ++ y.warnings
        , errors = x.errors ++ y.errors
        }

instance Monoid CompileState where
    mempty = CompileState
        { warnings = []
        , errors = []
        }

-- | A custom monad for compilation state as a CYIO-replacement that doesn't track errors in an ExceptT.
type CMT m = MaybeT (StateT CompileState (ReaderT CompileAuxiliary m))

runCMT :: MonadIO m => CMT m a -> CompileAuxiliary -> m (Maybe a, CompileState)
runCMT cm aux = flip runReaderT aux . flip runStateT mempty . runMaybeT $ cm

catchCYIO :: MonadIO m => CYIO a -> CMT m (Maybe a)
catchCYIO cyio = liftIO (runCYIO cyio) >>= \case
    Left es       -> do
        modify $ \s -> s { errors = s.errors ++ es }
        return Nothing
    Right (x, ws) -> do
        modify $ \s -> s { warnings = s.warnings ++ ws }
        return $ Just x

liftToCM :: Monad m => m a -> CMT m a
liftToCM = lift . lift . lift

liftCYIO :: MonadIO m => CYIO a -> CMT m a
liftCYIO = MaybeT . (join <$>) . runMaybeT . catchCYIO

type CompileOutput = [(FilePath, CE.CompEnv ModuleAST)]

-- | Compiles a Curry source file with its dependencies
-- using the given import paths and the given output directory
-- (in which the interface file will be placed). If compilation fails the
-- result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurryFileWithDeps :: (MonadIO m, MonadLsp CFG.Config m) => CFG.Config -> CompileAuxiliary -> [FilePath] -> FilePath -> FilePath -> m (CompileOutput, CompileState)
compileCurryFileWithDeps cfg aux importPaths outDirPath filePath = (fromMaybe mempty <.$>) $ flip runCMT aux $ do
    let defOpts = CO.defaultOptions
        cppOpts = CO.optCppOpts defOpts
        cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
        opts = CO.defaultOptions { CO.optForce = cfg.forceRecompilation
                                 , CO.optImportPaths = importPaths ++ cfg.importPaths
                                 , CO.optLibraryPaths = cfg.libraryPaths
                                 , CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs }
                                 , CO.optExtensions = nub $ CSE.kielExtensions ++ CO.optExtensions defOpts
                                 , CO.optOriginPragmas = True
                                 }
    -- Resolve dependencies
    deps <- liftCYIO $ CD.flatDeps opts filePath
    liftToCM $ debugM $ "Compiling " <> T.pack (takeFileName filePath) <> ", found deps: " <> T.intercalate ", " (ppToText . fst <$> deps)
    -- Compile the module and its dependencies in topological order
    compileCurryModules opts outDirPath deps

-- | Compiles the given list of modules in order.
compileCurryModules :: (MonadIO m, MonadLsp CFG.Config m) => CO.Options -> FilePath -> [(CI.ModuleIdent, CD.Source)] -> CMT m CompileOutput
compileCurryModules opts outDirPath deps = case deps of
    [] -> liftCYIO $ failMessages [makeFailMessage "Language Server: No module found"]
    ((m, CD.Source fp ps _is):ds) -> do
        liftToCM $ debugM $ "Actually compiling " <> T.pack fp
        opts' <- liftCYIO $ CB.processPragmas opts ps
        output <- compileCurryModule opts' outDirPath m fp
        if null ds
            then return output
            else (output <>) <$> compileCurryModules opts outDirPath ds
    (_:ds) -> compileCurryModules opts outDirPath ds

-- | Compiles a single module.
compileCurryModule :: (MonadIO m, MonadLsp CFG.Config m) => CO.Options -> FilePath -> CI.ModuleIdent -> FilePath -> CMT m CompileOutput
compileCurryModule opts outDirPath m fp = do
    liftToCM $ debugM $ "Compiling module " <> T.pack (takeFileName fp)
    -- Parse and check the module
    mdl <- loadAndCheckCurryModule opts m fp
    errs <- gets (.errors)
    when (null errs) $ do
        -- Generate and store an on-disk interface file
        mdl' <- CC.expandExports opts mdl
        interf <- liftCYIO $ uncurry (CEX.exportInterface opts) $ CT.qual mdl'
        let interfFilePath = outDirPath </> CFN.interfName (CFN.moduleNameToFile m)
            generated = PP.render $ CS.pPrint interf
        liftToCM $ debugM $ "Writing interface file to " <> T.pack interfFilePath
        liftIO $ CF.writeModule interfFilePath generated 
    return [(fp, mdl)]

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
loadAndCheckCurryModule :: (MonadIO m, MonadLsp CFG.Config m) => CO.Options -> CI.ModuleIdent -> FilePath -> CMT m (CE.CompEnv ModuleAST)
loadAndCheckCurryModule opts m fp = do
    -- Read source file (possibly from VFS)
    fl <- asks (.fileLoader)
    src <- liftIO $ fl fp
    -- Load and check module
    loaded <- liftCYIO $ loadCurryModule opts m src fp
    checked <- catchCYIO $ CMD.checkModule opts loaded
    liftCYIO $ warnMessages $ maybe [] (uncurry (CC.warnCheck opts)) checked
    let ast = maybe (Nothing <$ snd loaded) ((Just <$>) . snd) checked
        env = maybe (fst loaded) fst checked
    return (env, ast)

-- | Loads a single module.
loadCurryModule :: CO.Options -> CI.ModuleIdent -> String -> FilePath -> CYIO (CE.CompEnv (CS.Module()))
loadCurryModule opts m src fp = do
    -- Parse the module
    (lexed, ast) <- parseCurryModule opts m src fp
    -- Load the imported interfaces into an InterfaceEnv
    let paths = CFN.addOutDir (CO.optUseOutDir opts) (CO.optOutDir opts) <$> ("." : CO.optImportPaths opts)
    let withPrelude = importCurryPrelude opts ast
    iEnv <- CIF.loadInterfaces paths withPrelude
    checkInterfaces opts iEnv
    is <- importSyntaxCheck iEnv withPrelude
    -- Add Information of imported modules
    cEnv <- CIM.importModules withPrelude iEnv is
    return (cEnv { CE.filePath = fp, CE.tokens = lexed }, ast)

-- | Checks all interfaces.
checkInterfaces :: Monad m => CO.Options -> CEI.InterfaceEnv -> CYT m ()
checkInterfaces opts iEnv = mapM_ checkInterface $ M.elems iEnv
    where checkInterface intf = do
            let env = CIM.importInterfaces intf iEnv
            CC.interfaceCheck opts (env, intf)

-- | Checks all imports in the module.
importSyntaxCheck :: Monad m => CEI.InterfaceEnv -> CS.Module a -> CYT m [CS.ImportDecl]
importSyntaxCheck iEnv (CS.Module _ _ _ _ _ is _) = mapM checkImportDecl is
    where checkImportDecl (CS.ImportDecl p m q asM is') = case M.lookup m iEnv of
            Just intf -> CS.ImportDecl p m q asM `fmap` CC.importCheck intf is'
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
parseCurryModule opts _ src fp = do
    ul <- liftCYM $ CUL.unlit fp src
    -- TODO: Preprocess
    cc <- CNC.condCompile (CO.optCppOpts opts) fp ul
    lexed <- liftCYM $ silent $ CS.lexSource fp cc
    ast <- liftCYM $ CS.parseModule fp cc
    -- TODO: Check module/file mismatch?
    return (lexed, ast)

failedCompilation :: String -> (CompileOutput, CompileState)
failedCompilation msg = (mempty, mempty { errors = [makeFailMessage msg] })

makeFailMessage :: String -> CM.Message
makeFailMessage = CM.message . PP.text

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
module Curry.LanguageServer.Index.Store
    ( ModuleStoreEntry (..)
    , IndexStore (..)
    , storedModuleCount
    , storedSymbolCount
    , storedModule
    , storedModuleByIdent
    , storedModules
    , storedSymbols
    , storedSymbolsWithPrefix
    , storedSymbolsByQualIdent
    , storedModuleSymbolsByModuleIdent
    , storedModuleSymbolsWithPrefix
    , addWorkspaceDir
    , recompileModule
    , getModuleCount
    , getModule
    , getModuleList
    , getModuleAST
    ) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Base.Ident as CI
import qualified Curry.Base.Message as CM
import qualified Curry.Files.Filenames as CFN
import qualified Base.TopEnv as CT
import qualified CompilerEnv as CE

import Control.Exception (SomeException)
import Control.Monad (forM_, join, void, unless, filterM)
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad.Extra (whenM)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Curry.LanguageServer.Compiler as C
import Curry.LanguageServer.CPM.Deps (generatePathsJsonWithCPM, readPathsJson)
import Curry.LanguageServer.CPM.Monad (runCPMM)
import qualified Curry.LanguageServer.Config as CFG
import Curry.LanguageServer.Index.Convert
import Curry.LanguageServer.Index.Symbol
import Curry.LanguageServer.Utils.Convert
import Curry.LanguageServer.Utils.General
import Curry.LanguageServer.Utils.Logging (infoM, debugM, warnM)
import Curry.LanguageServer.Utils.Sema (ModuleAST)
import Curry.LanguageServer.Utils.Syntax (moduleIdentifier)
import Curry.LanguageServer.Utils.Uri
import Data.Default
import Data.Function (on)
import Data.List (unionBy, isPrefixOf, foldl')
import Data.List.Extra (nubOrdOn, nubOrd)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as TR
import qualified Language.LSP.Protocol.Types as J
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>), (</>), takeDirectory, takeExtension, takeFileName)
import qualified System.FilePath.Glob as G
import System.Process (readProcessWithExitCode)
import Language.LSP.Server (MonadLsp)

-- | An index store entry containing the parsed AST, the compilation environment
-- and diagnostic messages.
data ModuleStoreEntry = ModuleStoreEntry { moduleAST :: Maybe ModuleAST
                                         , errorMessages :: [CM.Message]
                                         , warningMessages :: [CM.Message]
                                         , projectDir :: Maybe FilePath
                                         , importPaths :: [FilePath]
                                         }



-- | An in-memory map of URIs to parsed modules and
-- unqualified symbol names to actual symbols/symbol information.
-- Since (unqualified) symbol names can be ambiguous, a trie leaf
-- holds a list of symbol entries rather than just a single one.
data IndexStore = IndexStore { modules :: M.Map J.NormalizedUri ModuleStoreEntry
                               -- Symbols keyed by unqualified name
                             , symbols :: TR.Trie [Symbol]
                               -- Module symbols keyed by qualified name
                             , moduleSymbols :: TR.Trie [Symbol]
                             }

instance Default ModuleStoreEntry where
    def = ModuleStoreEntry { moduleAST = Nothing
                           , warningMessages = []
                           , errorMessages = []
                           , projectDir = Nothing
                           , importPaths = []
                           }

instance Default IndexStore where
    def = IndexStore { modules = M.empty, symbols = TR.empty, moduleSymbols = TR.empty }

-- | Fetches the number of stored modules.
storedModuleCount :: IndexStore -> Int
storedModuleCount = M.size . (.modules)

-- | Fetches the number of stored symbols.
storedSymbolCount :: IndexStore -> Int
storedSymbolCount = TR.size . (.symbols)

-- | Fetches the given entry in the store.
storedModule :: J.NormalizedUri -> IndexStore -> Maybe ModuleStoreEntry
storedModule uri = M.lookup uri . (.modules)

-- | Fetches an entry in the store by module identifier.
storedModuleByIdent :: CI.ModuleIdent -> IndexStore -> IO (Maybe ModuleStoreEntry)
storedModuleByIdent mident store = flip storedModule store <$> uri
    where filePath = CFN.moduleNameToFile mident <.> "curry"
          uri = filePathToNormalizedUri filePath

-- | Fetches the entries in the store as a list.
storedModules :: IndexStore -> [(J.NormalizedUri, ModuleStoreEntry)]
storedModules = M.toList . (.modules)

-- | Fetches all symbols.
storedSymbols :: IndexStore -> [Symbol]
storedSymbols = join . TR.toListBy (const id) . (.symbols)

-- | Fetches the given (unqualified) symbol names in the store.
storedSymbolsByKey :: T.Text -> IndexStore -> [Symbol]
storedSymbolsByKey t = join . maybeToList . TR.lookup (TE.encodeUtf8 t) . (.symbols)

-- | Fetches the list of symbols starting with the given prefix.
storedSymbolsWithPrefix :: T.Text -> IndexStore -> [Symbol]
storedSymbolsWithPrefix pre = join . TR.elems . TR.submap (TE.encodeUtf8 pre) . (.symbols)

-- | Fetches stored symbols by qualified identifier.
storedSymbolsByQualIdent :: CI.QualIdent -> IndexStore -> [Symbol]
storedSymbolsByQualIdent q = filter ((== ppToText q) . (.qualIdent)) . storedSymbolsByKey name
    where name = T.pack $ CI.idName $ CI.qidIdent q

-- | Fetches the given (qualified) module symbol names in the store.
storedModuleSymbolsByKey :: T.Text -> IndexStore -> [Symbol]
storedModuleSymbolsByKey t = join . maybeToList . TR.lookup (TE.encodeUtf8 t) . (.moduleSymbols)

-- | Fetches stored symbols by qualified identifier.
storedModuleSymbolsByModuleIdent :: CI.ModuleIdent -> IndexStore -> [Symbol]
storedModuleSymbolsByModuleIdent = storedModuleSymbolsByKey . ppToText

-- | Fetches stored module symbols starting with the given prefix.
storedModuleSymbolsWithPrefix :: T.Text -> IndexStore -> [Symbol]
storedModuleSymbolsWithPrefix pre = join . TR.elems . TR.submap (TE.encodeUtf8 pre) . (.moduleSymbols)

-- | Compiles the given directory recursively and stores its entries.
addWorkspaceDir :: (MonadState IndexStore m, MonadIO m, MonadLsp CFG.Config m, MonadCatch m) => CFG.Config -> C.FileLoader -> FilePath -> m ()
addWorkspaceDir cfg fl dirPath = void $ runMaybeT $ do
    files <- lift $ findCurrySourcesInWorkspace cfg dirPath
    lift $ do
        mapM_ (\(i, file) -> recompileFile i (length files) cfg fl file.importPaths (Just file.projectDir) file.path) (zip [1..] files)
        infoM $ "Added workspace directory " <> T.pack dirPath

-- | Recompiles the module entry with the given URI and stores the output.
recompileModule :: (MonadState IndexStore m, MonadIO m, MonadLsp CFG.Config m, MonadCatch m) => CFG.Config -> C.FileLoader -> J.NormalizedUri -> m ()
recompileModule cfg fl uri = void $ runMaybeT $ do
    filePath <- liftMaybe $ J.uriToFilePath $ J.fromNormalizedUri uri
    lift $ do
        recompileFile 1 1 cfg fl [] Nothing filePath
        debugM $ "Recompiled entry " <> T.pack (show uri)

data CurrySourceFile = CurrySourceFile { projectDir :: FilePath
                                       , importPaths :: [FilePath]
                                       , path :: FilePath
                                       }

-- | Finds the Curry source files along with its import paths in a workspace. Recognizes CPM projects.
findCurrySourcesInWorkspace :: (MonadIO m, MonadLsp CFG.Config m) => CFG.Config -> FilePath -> m [CurrySourceFile]
findCurrySourcesInWorkspace cfg dirPath = do
    -- First and foremost, the language server tries to locate CPM packages by their 'package.json'
    cpmProjPaths <- walkCurryProjects ["package.json"] dirPath
    -- In addition to that, it also supports non-CPM packages located at '.curry/language-server/paths.json'
    pathsJsonProjPaths <- walkCurryProjects [".curry", "language-server", "paths.json"] dirPath
    -- If nothing is found, default to the workspace directory
    let projPaths = fromMaybe [dirPath] $ nothingIfNull $ nubOrd $ cpmProjPaths ++ pathsJsonProjPaths
    nubOrdOn (.path) . join <$> mapM (findCurrySourcesInProject cfg) projPaths

-- | Finds the Curry source files in a (project) directory.
findCurrySourcesInProject :: (MonadIO m, MonadLsp CFG.Config m) => CFG.Config -> FilePath -> m [CurrySourceFile]
findCurrySourcesInProject cfg dirPath = do
    let curryPath = cfg.curryPath
        cpmPath = curryPath ++ " cypm"
        libPath binPath = takeDirectory (takeDirectory binPath) </> "lib"

    infoM $ "Entering project " <> T.pack dirPath <> "..."

    whenM (liftIO $ doesFileExist $ dirPath </> "package.json") $ do
        infoM "Resolving dependencies automatically since package.json was found..."
        cpmResult <- runCPMM $ generatePathsJsonWithCPM dirPath cpmPath
        case cpmResult of
            Right _ -> infoM $ "Successfully updated paths.json using '" <> T.pack cpmPath <> "'!"
            Left _  -> infoM $ "Could not update paths.json using " <> T.pack cpmPath <> ", trying to read paths.json anyway..."

    infoM "Reading paths.json..."
    pathsResult <- runCPMM $ readPathsJson dirPath
    paths <- case pathsResult of
        Right paths -> do
            infoM $ "Successfully read paths.json: " <> T.pack (show (length paths)) <> " path(s)"
            return paths
        Left e      -> do
            warnM $ "Could not read paths.json (" <> T.pack e <> "), trying fallback resolution of Curry standard libraries..."
            (exitCode, fullCurryPath, _) <- liftIO $ readProcessWithExitCode "which" [curryPath] []
            let curryLibPath = libPath fullCurryPath

            if exitCode == ExitSuccess then do
                warnM "Could not find Curry standard libraries, this might result in 'missing Prelude' errors..."
                return []
            else do
                infoM $ "Found Curry standard library at " <> T.pack curryLibPath
                return [curryLibPath]
    
    infoM "Searching for sources..."
    projSources <- walkCurrySourceFiles dirPath

    return $ CurrySourceFile dirPath paths <$> projSources

-- | Recursively finds all projects in a directory containing the given identifying file.
walkCurryProjects :: (MonadIO m, MonadLsp CFG.Config m) => [FilePath] -> FilePath -> m [FilePath]
walkCurryProjects relPath dirPath = do
    files <- walkIgnoringHidden dirPath
    filterM (liftIO . doesFileExist . applyRelPath) files
    where applyRelPath = flip (foldl' (</>)) relPath

-- | Recursively finds all Curry source files in a directory.
walkCurrySourceFiles :: (MonadIO m, MonadLsp CFG.Config m) => FilePath -> m [FilePath]
walkCurrySourceFiles = (filter ((== ".curry") . takeExtension) <$>) . walkIgnoringHidden

-- | Recursively finds Curry source files, ignoring directories starting with dots
--   and those specified in .curry-language-server-ignore.
--   TODO: Respect parent gitignore also in subdirectories (may require changes to walkFilesWith
--         to aggregate the state across recursive calls, perhaps by requiring a Monoid instance?)
walkIgnoringHidden :: (MonadIO m, MonadLsp CFG.Config m) => FilePath -> m [FilePath]
walkIgnoringHidden = walkFilesWith WalkConfiguration
    { onEnter            = \fp -> do
        ignorePaths <- filterM (liftIO . doesFileExist) $ (fp </>) <$> [".curry-language-server-ignore", ".gitignore"]
        ignored     <- join <$> mapM readIgnoreFile ignorePaths
        unless (null ignored) $
            infoM $ "In '" <> T.pack (takeFileName fp) <> "' ignoring " <> T.pack (show (G.decompile <$> ignored))
        return $ Just ignored
    , shouldIgnore       = \ignored fp -> do
        isDir <- liftIO $ doesDirectoryExist fp
        let fn              = takeFileName fp
            matchesFn pat   = any (G.match pat) $ catMaybes [Just fn, if isDir then Just (fn ++ "/") else Nothing]
            matchingIgnores = filter matchesFn ignored
        unless (null matchingIgnores) $
            debugM $ "Ignoring '" <> T.pack fn <> "' since it matches " <> T.pack (show (G.decompile <$> matchingIgnores))
        return $ not (null matchingIgnores) || "." `isPrefixOf` fn
    , includeDirectories = True
    , includeFiles       = True
    }

-- | Reads the given ignore file, fetching the ignored (relative) paths.
readIgnoreFile :: MonadIO m => FilePath -> m [G.Pattern]
readIgnoreFile = liftIO . (map (G.simplify . G.compile . T.unpack) . filter useLine . T.lines <$>) . TIO.readFile
    where useLine l = not (T.null l) && not ("#" `T.isPrefixOf` l)

-- | Recompiles the entry with its dependencies using explicit paths and stores the output.
recompileFile :: (MonadState IndexStore m, MonadIO m, MonadLsp CFG.Config m, MonadCatch m) => Int -> Int -> CFG.Config -> C.FileLoader -> [FilePath] -> Maybe FilePath -> FilePath -> m ()
recompileFile i total cfg fl importPaths dirPath filePath = void $ do
    infoM $ "[" <> T.pack (show i) <> " of " <> T.pack (show total) <> "] (Re)compiling file " <> T.pack (takeFileName filePath)

    uri <- filePathToNormalizedUri filePath
    ms <- gets (.modules)

    -- Regarding the ambiguous-fields warning, perhaps this is https://gitlab.haskell.org/ghc/ghc/-/issues/21443 ?
    let defEntry = (def { projectDir = dirPath, importPaths = importPaths }) :: ModuleStoreEntry
        outDirPath = CFN.defaultOutDir </> "language-server"
        importPaths' = outDirPath : (M.findWithDefault defEntry uri ms).importPaths
        aux = C.CompileAuxiliary { C.fileLoader = fl }

    (co, cs) <- catch
        (C.compileCurryFileWithDeps cfg aux importPaths' outDirPath filePath)
        (\e -> return $ C.failedCompilation $ "Compilation failed: " ++ show (e :: SomeException))

    let msgNormUri msg = (fromMaybe uri <$>) $ runMaybeT $ do
            uri' <- currySpanInfo2Uri $ CM.msgSpanInfo msg
            normalizeUriWithPath uri'

    -- Ignore parses from interface files, only consider source files for now
    asts <- mapM (\(fp, mdl) -> (, mdl) <$> filePathToNormalizedUri fp) $ filter ((".curry" `T.isSuffixOf`) . T.pack . fst) co

    warns  <- groupIntoMapByM msgNormUri cs.warnings
    errors <- groupIntoMapByM msgNormUri cs.errors

    debugM $ "Recompiled module paths: " <> T.pack (show (fst <$> asts))

    -- Update store with compiled modules

    let modifyEntry f = M.alter (Just . f . fromMaybe defEntry)

    forM_ asts $ \(uri', (env, ast)) -> do
        -- Update module store
        let updateEntry e = e
                { warningMessages = M.findWithDefault [] uri' warns
                , errorMessages = M.findWithDefault [] uri' errors
                , moduleAST = Just ast
                -- , mseCompilerEnv = Just env
                }
        modify $ \s -> s { modules = modifyEntry updateEntry uri' s.modules }

        -- Update symbol store
        valueSymbols <- join <$> mapM toSymbols (CT.allBindings $ CE.valueEnv env)
        typeSymbols  <- join <$> mapM toSymbols (CT.allBindings $ CE.tyConsEnv env)
        modSymbols   <- toSymbols (moduleIdentifier ast)

        let symbolDelta = valueSymbols ++ typeSymbols ++ modSymbols
            combiner = unionBy ((==) `on` (\s' -> (s'.kind, s'.qualIdent, symbolIsFromCurrySource s')))
        modify $ \s -> s
            { symbols = insertAllIntoTrieWith combiner ((\s' -> (TE.encodeUtf8 s'.ident, [s'])) <$> symbolDelta) s.symbols
            , moduleSymbols = insertAllIntoTrieWith (unionBy ((==) `on` (.qualIdent))) ((\s' -> (TE.encodeUtf8 s'.qualIdent, [s'])) <$> modSymbols) s.moduleSymbols
            }

    -- Update store with messages from files that were not successfully compiled

    let uris = S.fromList $ fst <$> asts
        other = filter ((`S.notMember` uris) . fst) . M.toList

    forM_ (other warns) $ \(uri', msgs) -> do
        let updateEntry e = e { warningMessages = msgs }
        modify $ \s -> s { modules = modifyEntry updateEntry uri' s.modules }

    forM_ (other errors) $ \(uri', msgs) -> do
        let updateEntry e = e { errorMessages = msgs }
        modify $ \s -> s { modules = modifyEntry updateEntry uri' s.modules }

-- | Fetches the number of module entries in the store in a monadic way.
getModuleCount :: (MonadState IndexStore m) => m Int
getModuleCount = gets storedModuleCount

-- | Fetches a module entry in the store in a monadic way.
getModule :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleStoreEntry
getModule uri = liftMaybe =<< gets (storedModule uri)

-- | Fetches the module entries in the store as a list in a monadic way.
getModuleList :: (MonadState IndexStore m) => m [(J.NormalizedUri, ModuleStoreEntry)]
getModuleList = gets storedModules

-- | Fetches the AST for a given URI in the store in a monadic way.
getModuleAST :: (MonadState IndexStore m) => J.NormalizedUri -> MaybeT m ModuleAST
getModuleAST uri = (liftMaybe . (.moduleAST)) =<< getModule uri

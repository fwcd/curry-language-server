module Curry.LanguageServer.Compiler (
    ModuleAST,
    CompilationOutput (..),
    CompilationResult,
    compileCurry,
    parseInterface,
    compilationToMaybe,
    failedCompilation
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Files.PathUtils as CF
import Curry.Base.Message as CM
import Curry.Base.Monad (CYIO, runCYIO, runCYM, failMessages)
import Curry.Base.Position as CP
import qualified Curry.Syntax as CS
import qualified Base.Types as CT
import qualified CurryDeps as CD
import qualified CompilerEnv as CE
import qualified CompilerOpts as CO
import Modules (loadAndCheckModule)
import qualified Text.PrettyPrint as PP

import Control.Monad.Reader
import Curry.LanguageServer.Logging
import Curry.LanguageServer.Utils.General
import qualified Data.Map as M
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Language.Haskell.LSP.Types as J
import System.Directory
import System.FilePath

type ModuleAST = CS.Module CT.PredType -- TODO: PredType renamed to type in later versions of curry-frontend
data CompilationOutput = CompilationOutput { compilerEnv :: CE.CompilerEnv, moduleAST :: ModuleAST }
type CompilationResult = Either [CM.Message] (CompilationOutput, [CM.Message])

-- | Compiles Curry code using the given import path. If compilation
-- fails the result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurry :: [String] -> FilePath -> IO CompilationResult
compileCurry importPaths filePath = runCYIO $ do
    deps <- CD.flatDeps opts filePath
    liftIO $ logs DEBUG $ "Compiling Curry, found deps: " ++ show deps
    mdl <- justOrFail "Language Server: No module found" $ listToMaybe $ map fst $ filter (depMatches filePath . snd) $ deps
    toCompilationOutput <$> loadAndCheckModule opts mdl filePath
    where cppOpts = CO.optCppOpts CO.defaultOptions
          cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
          opts = CO.defaultOptions { CO.optForce = True,
                                     CO.optImportPaths = importPaths,
                                     CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }

parseInterface :: FilePath -> IO (Maybe CS.Interface)
parseInterface fp = do
    logs DEBUG $ "parseInterface: Parsing interface at " ++ fp
    src <- CF.readModule fp
    return $ (eitherToMaybe . (fst <$>) . runCYM . CS.parseInterface fp) =<< src

compilationToMaybe :: CompilationResult -> Maybe CompilationOutput
compilationToMaybe = (fst <$>) . eitherToMaybe

depMatches :: FilePath -> CD.Source -> Bool
depMatches fp1 (CD.Source fp2 _ _) = fp1 == fp2
depMatches _ _ = False

toCompilationOutput :: CE.CompEnv ModuleAST -> CompilationOutput
toCompilationOutput (env, ast) = CompilationOutput { compilerEnv = env, moduleAST = ast }

justOrFail :: String -> Maybe a -> CYIO a
justOrFail _ (Just x) = return x
justOrFail msg Nothing = failMessages [failMessageFrom msg]

failedCompilation :: String -> CompilationResult
failedCompilation msg = Left [failMessageFrom msg]

failMessageFrom :: String -> CM.Message
failMessageFrom = CM.message . PP.text

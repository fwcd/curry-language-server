module Curry.LanguageServer.Compiler (
    CompilationOutput (..),
    CompilationResult,
    ConcreteCompilationResult,
    compileCurry,
    compilationToMaybe,
    failedCompilation
) where

-- Curry Compiler Libraries + Dependencies
import qualified Curry.Files.Filenames as CF
import Curry.Base.Message as CM
import Curry.Base.Monad (CYIO, runCYIO, failMessages)
import Curry.Base.Position as CP
import qualified Curry.Syntax as CS
import qualified Base.Types as CT
import qualified CurryDeps as CD
import qualified CompilerEnv as CE
import qualified CompilerOpts as CO
import Modules (loadAndCheckModule)
import qualified Text.PrettyPrint as PP

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe)
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

data CompilationOutput a = CompilationOutput { compilerEnv :: CE.CompilerEnv, moduleAST :: CS.Module a }
type CompilationResult a = Either [CM.Message] (CompilationOutput a, [CM.Message])
type ConcreteCompilationResult = CompilationResult CT.PredType -- TODO: PredType renamed to type in later versions of curry-frontend

-- | Compiles Curry code using the given import path. If compilation
-- fails the result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurry :: [String] -> FilePath -> IO ConcreteCompilationResult
compileCurry importPaths filePath = runCYIO $ do
    deps <- CD.flatDeps opts filePath
    liftIO $ U.logs $ "Compiling Curry, found deps: " ++ show deps
    mdl <- justOrFail "Language Server: No module found" $ listToMaybe $ map fst $ filter (depMatches filePath . snd) $ deps
    toCompilationOutput <$> loadAndCheckModule opts mdl filePath
    where cppOpts = CO.optCppOpts CO.defaultOptions
          cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
          opts = CO.defaultOptions { CO.optForce = True,
                                     CO.optImportPaths = importPaths,
                                     CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }

compilationToMaybe :: CompilationResult a -> Maybe (CompilationOutput a)
compilationToMaybe = (fst <$>) . eitherToMaybe

depMatches :: FilePath -> CD.Source -> Bool
depMatches fp1 (CD.Source fp2 _ _) = fp1 == fp2
depMatches _ _ = False

toCompilationOutput :: CE.CompEnv (CS.Module a) -> CompilationOutput a
toCompilationOutput (env, ast) = CompilationOutput { compilerEnv = env, moduleAST = ast }

justOrFail :: String -> Maybe a -> CYIO a
justOrFail _ (Just x) = return x
justOrFail msg Nothing = failMessages [failMessageFrom msg]

failedCompilation :: String -> CompilationResult a
failedCompilation msg = Left [failMessageFrom msg]

failMessageFrom :: String -> CM.Message
failMessageFrom = CM.message . PP.text

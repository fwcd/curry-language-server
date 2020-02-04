module Curry.LanguageServer.Compiler (compileCurry) where

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
import Data.Maybe
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U

-- TODO: CT.PredType has been renamed to CT.Type in newer versions of curry-frontend
data CompilationOutput = CompilationOutput { compilerEnv :: CE.CompilerEnv, moduleAST :: CS.Module (CT.PredType) }
type CompilationResult = Either [CM.Message] (CompilationOutput, [CM.Message])

-- | Compiles Curry code using the given import path. If compilation
-- fails the result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurry :: [String] -> FilePath -> IO CompilationResult
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

depMatches :: FilePath -> CD.Source -> Bool
depMatches fp1 (CD.Source fp2 _ _) = fp1 == fp2
depMatches _ _ = False

justOrFail :: String -> Maybe a -> CYIO a
justOrFail _ (Just x) = return x
justOrFail msg Nothing = failMessages [CM.message $ PP.text msg]

-- TODO: CT.PredType has been renamed to CT.Type in newer versions of curry-frontend
toCompilationOutput :: CE.CompEnv (CS.Module (CT.PredType)) -> CompilationOutput
toCompilationOutput (env, ast) = CompilationOutput { compilerEnv = env, moduleAST = ast }

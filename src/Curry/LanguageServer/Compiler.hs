module Curry.LanguageServer.Compiler (compileCurry) where

-- Curry Compiler Libraries + Dependencies
import Curry.Base.Message as CM
import Curry.Base.Position as CP
import Curry.Base.Monad (runCYIO, failMessages)
import qualified Curry.Syntax as CS
import qualified Base.Types as CT
import qualified CurryDeps as CD
import qualified CompilerEnv as CE
import qualified CompilerOpts as CO
import Modules (loadAndCheckModule)
import qualified Text.PrettyPrint as PP

import qualified Data.Map as M
import Data.Maybe
import qualified Language.Haskell.LSP.Types as J

-- TODO: CT.PredType has been renamed to CT.Type in newer versions of curry-frontend
data CompilationOutput = CompilationOutput { compilerEnv :: CE.CompilerEnv, moduleAST :: CS.Module (CT.PredType) }
type CompilationResult = Either [CM.Message] (CompilationOutput, [CM.Message])

-- | Compiles Curry code using the given import path. If compilation
-- fails the result will be `Left` and contain error messages.
-- Otherwise it will be `Right` and contain both the parsed AST and
-- warning messages.
compileCurry :: [String] -> FilePath -> IO CompilationResult
compileCurry importPaths filePath = runCYIO $ do
        srcs <- CD.flatDeps opts filePath
        case srcs of
            ((mdl, _):_) -> toCompilationOutput <$> loadAndCheckModule opts mdl filePath
            _ -> failMessages [CM.message $ PP.text "Curry Language Server: Could not find module using flatDeps"]
    where cppOpts = CO.optCppOpts CO.defaultOptions
          cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
          opts = CO.defaultOptions { CO.optForce = True,
                                     CO.optImportPaths = importPaths,
                                     CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }

-- TODO: CT.PredType has been renamed to CT.Type in newer versions of curry-frontend
toCompilationOutput :: CE.CompEnv (CS.Module (CT.PredType)) -> CompilationOutput
toCompilationOutput (env, ast) = CompilationOutput { compilerEnv = env, moduleAST = ast }

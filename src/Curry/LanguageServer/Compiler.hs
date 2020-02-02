module Curry.LanguageServer.Compiler (compileCurry) where

-- Curry Compiler Libraries + Dependencies
import Curry.Base.Message as CM
import Curry.Base.Position as CP
import Curry.Base.Monad (runCYIO)
import qualified Curry.Syntax as CS
import CurryBuilder as CB
import CompilerOpts as CO

import qualified Data.Map as M
import Data.Maybe
import qualified Language.Haskell.LSP.Types as J

compileCurry :: [String] -> FilePath -> IO (Either [CM.Message] ((), [CM.Message]))
compileCurry importPaths filePath = runCYIO $ buildCurry opts filePath
    where cppOpts = CO.optCppOpts CO.defaultOptions
          cppDefs = M.insert "__PAKCS__" 300 (CO.cppDefinitions cppOpts)
          opts = CO.defaultOptions { CO.optForce = True,
                                      CO.optImportPaths = importPaths,
                                      CO.optCppOpts = cppOpts { CO.cppDefinitions = cppDefs } }

-- TODO: Use loadAndCheckModule to fetch an AST and messages directly

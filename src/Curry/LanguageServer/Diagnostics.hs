module Curry.LanguageServer.Diagnostics where

-- Curry Compiler Libraries + Dependencies
import Curry.Base.Message as CM
import Curry.Base.Position as CP
import Curry.Base.Monad (runCYIO)
import qualified Curry.Syntax as CS
import qualified Text.PrettyPrint as PP
import Modules
import CompilerOpts as CO
import CurryDeps as CD

import Control.Monad
import Control.Monad.Reader
import Curry.LanguageServer.Aliases
import Data.Maybe
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Types as J

fetchDiagnostics :: J.NormalizedUri -> RM () [J.Diagnostic]
fetchDiagnostics doc = do
    msgs <- liftIO $ runCYIO $ do
        -- TODO: Layer a MaybeT monad transformer to make this section more pretty
        case J.uriToFilePath $ J.fromNormalizedUri doc of
            Just filePath -> do
                let opts = CO.defaultOptions
                deps <- flatDeps opts filePath
                -- TODO: Generate diagnostics for all sources in the project,
                --       not just the first one the dependencies list (which could
                --       be arbitrary).
                case listToMaybe deps of
                    Just (mident, _) -> compileModule opts mident filePath
                    Nothing -> return ()
            Nothing -> return ()
    let diags = case (msgs :: Either [CM.Message] ((), [CM.Message])) of
                    Left errs -> map (curryMsg2Diagnostic J.DsError) errs
                    Right (_, warns) -> map (curryMsg2Diagnostic J.DsWarning) warns
    return diags

curryMsg2Diagnostic :: J.DiagnosticSeverity -> CM.Message -> J.Diagnostic
curryMsg2Diagnostic s msg = J.Diagnostic range severity code src text related
    where pos@(J.Position ln col) = maybe (J.Position 0 0) curryPos2Pos $ CM.msgPos msg
          -- TODO: Fetch a span from Curry compiler instead of just a position
          range = J.Range pos $ J.Position ln (col + 200)
          severity = Just s
          code = Nothing
          src = Nothing
          text = T.pack $ PP.render $ CM.msgTxt msg
          related = Nothing

-- TODO: Use (file :: FilePath) from Curry Position to accurately
--       map diagnostics to their respective files.
curryPos2Pos :: CP.Position -> J.Position
curryPos2Pos p = J.Position ((CP.line p) - 1) ((CP.column p) - 1)

module Curry.LanguageServer.Utils.Logging
    ( logAt, showAt
    , errorM, warnM, infoM, debugM
    ) where

import Colog.Core (Severity (..), WithSeverity (..), (<&))
import qualified Data.Text as T
import Language.LSP.Logging (logToLogMessage, logToShowMessage)
import Language.LSP.Server (MonadLsp)

-- | Logs a message to the output console (window/logMessage).
logAt :: MonadLsp c m => Severity -> T.Text -> m ()
logAt sev msg = logToLogMessage <& WithSeverity msg sev

-- | Presents a log message in a notification to the user (window/showMessage).
showAt :: MonadLsp c m => Severity -> T.Text -> m ()
showAt sev msg = logToShowMessage <& WithSeverity msg sev

-- | Logs a message at the error level. This presents an error notification to the user.
errorM :: MonadLsp c m => T.Text -> m ()
errorM = showAt Error

-- | Logs a message at the warning level.
warnM :: MonadLsp c m => T.Text -> m ()
warnM = logAt Warning

-- | Logs a message at the info level.
infoM :: MonadLsp c m => T.Text -> m ()
infoM = logAt Info

-- | Logs a message at the debug level.
debugM :: MonadLsp c m => T.Text -> m ()
debugM = logAt Debug

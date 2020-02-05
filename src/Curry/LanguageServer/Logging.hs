module Curry.LanguageServer.Logging (setupLogging) where

import qualified Data.Text as T
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types.MessageFuncs
import qualified System.Log as L
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import qualified System.Log.Logger as LL

data CLSLogHandler = CLSLogHandler { sendFunc :: Core.SendFunc, level :: L.Priority, formatter :: LF.LogFormatter CLSLogHandler }

instance LH.LogHandler CLSLogHandler where
    setLevel lh l = lh { level = l }
    getLevel = level
    setFormatter lh f = lh { formatter = f }
    getFormatter = formatter
    emit lh (prio, msg) _ | prio >= L.CRITICAL = sendFunc lh $ NotShowMessage $ fmServerShowMessageNotification (levelToMessageType $ level lh) $ T.pack msg
                          | otherwise          = sendFunc lh $ NotLogMessage $ fmServerLogMessageNotification (levelToMessageType $ level lh) $ T.pack msg
        where levelToMessageType l = case l of
                                        L.DEBUG -> J.MtLog
                                        L.INFO -> J.MtInfo
                                        L.NOTICE -> J.MtInfo
                                        L.WARNING -> J.MtWarning
                                        L.ERROR -> J.MtError
                                        L.CRITICAL -> J.MtError
                                        L.ALERT -> J.MtError
                                        L.EMERGENCY -> J.MtError
    close = const $ return ()

logName :: String
logName = "curry-language-server"

logFormat :: String
logFormat = "$time [$tid] - $msg"

logDateFormat :: String
logDateFormat = "%Y-%m-%d %H:%M:%S"

setupLogging :: Core.SendFunc -> L.Priority -> IO ()
setupLogging sf level = do
    let handler = CLSLogHandler { sendFunc = sf, level = level, formatter = LF.tfLogFormatter logName logDateFormat }
    LL.updateGlobalLogger LL.rootLoggerName $ LL.setHandlers ([] :: [CLSLogHandler])
    LL.updateGlobalLogger logName $ LL.setHandlers [handler] >> LL.setLevel level

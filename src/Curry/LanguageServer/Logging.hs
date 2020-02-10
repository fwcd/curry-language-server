{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Curry.LanguageServer.Logging (
    setupLogging,
    parseLogLevel,
    updateLogLevel,
    removeAllLogHandlers,
    Loggable (..),
    module System.Log
) where

import Control.Monad (void)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Constant as LSPConst
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import System.Log
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import qualified System.Log.Logger as LL

data CLSLogHandler = CLSLogHandler { sendFunc :: Core.SendFunc, level :: Priority, formatter :: LF.LogFormatter CLSLogHandler }

instance LH.LogHandler CLSLogHandler where
    setLevel lh l = lh { level = l }
    getLevel = level
    setFormatter lh f = lh { formatter = f }
    getFormatter = formatter
    emit lh (prio, msg) n | prio >= CRITICAL = sendFunc lh $ NotShowMessage $ fmServerShowMessageNotification (levelToMessageType $ level lh) $ T.pack msg
                          | otherwise        = sendFunc lh $ NotLogMessage $ fmServerLogMessageNotification (levelToMessageType $ level lh) $ T.pack msg
        where levelToMessageType l = case l of
                                        DEBUG -> J.MtInfo
                                        INFO -> J.MtInfo
                                        NOTICE -> J.MtInfo
                                        WARNING -> J.MtWarning
                                        ERROR -> J.MtError
                                        CRITICAL -> J.MtError
                                        ALERT -> J.MtError
                                        EMERGENCY -> J.MtError
    close = const $ return ()

logName :: String
logName = "curry-language-server"

logFormat :: String
logFormat = "$tid - $msg"

setupLogging :: Core.SendFunc -> IO ()
setupLogging sf = do
    let handler = CLSLogHandler { sendFunc = sf, level = level, formatter = LF.simpleLogFormatter logFormat }
        level = INFO
    updateLoggers $ LL.setHandlers [handler] <$> LL.setLevel level

updateLogLevel :: Priority -> IO ()
updateLogLevel = updateLoggers . LL.setLevel

updateLoggers :: (LL.Logger -> LL.Logger) -> IO ()
updateLoggers f = void $ sequence $ flip LL.updateGlobalLogger f <$> updatedLoggers
    where updatedLoggers = [LL.rootLoggerName, logName, LSPConst._LOG_NAME]

parseLogLevel :: String -> Priority
parseLogLevel s = case toLower <$> s of
    "debug" -> DEBUG
    "info" -> INFO
    "notice" -> NOTICE
    "warning" -> WARNING
    "error" -> ERROR
    "critical" -> CRITICAL
    "alert" -> ALERT
    "emergency" -> EMERGENCY
    _ -> INFO

removeAllLogHandlers :: IO ()
removeAllLogHandlers = LL.removeAllHandlers

class Loggable s where
    logs :: Priority -> s -> IO ()

instance Loggable String where
    logs p = LL.logM logName p

instance Loggable T.Text where
    logs p = logs p . T.unpack

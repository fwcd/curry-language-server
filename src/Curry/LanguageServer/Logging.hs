{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Curry.LanguageServer.Logging (
    setupLogging,
    removeAllLogHandlers,
    Loggable (..),
    module System.Log
) where

import qualified Data.Text as T
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types.MessageFuncs
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
logFormat = "$time [$tid] - $msg"

logDateFormat :: String
logDateFormat = "%Y-%m-%d %H:%M:%S"

setupLogging :: Core.SendFunc -> Priority -> IO ()
setupLogging sf level = do
    let handler = CLSLogHandler { sendFunc = sf, level = level, formatter = LF.tfLogFormatter logDateFormat logFormat }
    LL.updateGlobalLogger LL.rootLoggerName $ LL.setHandlers ([] :: [CLSLogHandler])
    LL.updateGlobalLogger logName $ LL.setHandlers [handler] <$> LL.setLevel level

removeAllLogHandlers :: IO ()
removeAllLogHandlers = LL.removeAllHandlers

class Loggable s where
    logs :: Priority -> s -> IO ()

instance Loggable String where
    logs p = LL.logM logName p

instance Loggable T.Text where
    logs p = logs p . T.unpack

{-# LANGUAGE FlexibleInstances, RankNTypes, KindSignatures, DataKinds #-}
module Curry.LanguageServer.LogHandler
    ( setupLogging
    , parseLogLevel
    , updateLogLevel
    , removeAllLogHandlers
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (askRunInIO)
import Curry.LanguageServer.Monad (LSM)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import System.Log (Priority (..))
import qualified System.Log.Formatter as LF
import qualified System.Log.Handler as LH
import qualified System.Log.Logger as LL

type SendNotification = forall (m :: J.Method 'J.FromServer 'J.Notification). J.SServerMethod m -> J.MessageParams m -> IO ()

data CLSLogHandler = CLSLogHandler { sendNotification :: SendNotification
                                   , level :: Priority
                                   , formatter :: LF.LogFormatter CLSLogHandler
                                   }

instance LH.LogHandler CLSLogHandler where
    setLevel lh l = lh { level = l }
    getLevel = level
    setFormatter lh f = lh { formatter = f }
    getFormatter = formatter
    emit lh (prio, msg) _ | prio >= CRITICAL = sendNotification lh J.SWindowShowMessage $ J.ShowMessageParams (levelToMessageType $ level lh) $ T.pack msg
                          | otherwise        = sendNotification lh J.SWindowLogMessage $ J.LogMessageParams (levelToMessageType $ level lh) $ T.pack msg
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
logName = "cls"

logFormat :: String
logFormat = "$loggername: $msg"

setupLogging :: LSM ()
setupLogging = do
    -- TODO: Ask for config with log level?
    runInIO <- askRunInIO
    let sn m = runInIO . S.sendNotification m
        lvl = INFO
        handler = CLSLogHandler { sendNotification = sn, level = lvl, formatter = LF.simpleLogFormatter logFormat }
    liftIO $ do
        LL.updateGlobalLogger LL.rootLoggerName $ LL.setHandlers ([] :: [CLSLogHandler])
        updateLoggers $ LL.setHandlers [handler] <$> LL.setLevel lvl

updateLogLevel :: Priority -> IO ()
updateLogLevel = updateLoggers . LL.setLevel

updateLoggers :: (LL.Logger -> LL.Logger) -> IO ()
updateLoggers f = mapM_ (`LL.updateGlobalLogger` f) updatedLoggers
    where updatedLoggers = [logName]

parseLogLevel :: String -> Maybe Priority
parseLogLevel s = case toLower <$> s of
    "debug" -> Just DEBUG
    "info" -> Just INFO
    "notice" -> Just NOTICE
    "warning" -> Just WARNING
    "error" -> Just ERROR
    "critical" -> Just CRITICAL
    "alert" -> Just ALERT
    "emergency" -> Just EMERGENCY
    _ -> Nothing

removeAllLogHandlers :: IO ()
removeAllLogHandlers = LL.removeAllHandlers

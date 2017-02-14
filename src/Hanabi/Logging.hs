{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Hanabi.Logging
  ( startLogging
  , stopLogging
  , logToStderr
  , logToFile
  , logExceptions
  , logDebug
  , logInfo
  , logWarn
  , withLogging
  , Logger
  ) where

import Control.Concurrent.Async (async, cancel, Async)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Logger as Logging
import Control.Monad.Logger (LoggingT, MonadLogger)
import Data.String.Conversions (convertString)
import Data.Text (Text)

startLogging
  :: MonadIO m
  => LogFunction -> m Logger
startLogging logFun = do
  chan <- liftIO Chan.newChan
  logThread <- liftIO (async (runLogFun logFun (Logging.unChanLoggingT chan)))
  return (Logger logThread chan)

logDebug
  :: (MonadLogger m, MonadIO m)
  => Text -> m ()
logDebug = Logging.logDebugN

logInfo
  :: (MonadLogger m, MonadIO m)
  => Text -> m ()
logInfo = Logging.logInfoN

logWarn
  :: (MonadLogger m, MonadIO m)
  => Text -> m ()
logWarn = Logging.logWarnN

withLogging
  :: MonadIO m
  => Logger -> LoggingT m a -> m a
withLogging (Logger _ chan) = Logging.runChanLoggingT chan

stopLogging
  :: MonadIO m
  => Logger -> m ()
stopLogging (Logger logThread _) = liftIO (cancel logThread)

data Logger =
  Logger (Async (LoggingT IO ()))
         LogChan

type LogChan = Chan (Logging.Loc, Logging.LogSource, Logging.LogLevel, Logging.LogStr)

data LogFunction
  = ToStderr
  | ToFile FilePath

logToStderr :: LogFunction
logToStderr = ToStderr

logToFile :: FilePath -> LogFunction
logToFile = ToFile

runLogFun
  :: MonadIO m
  => LogFunction -> LoggingT m a -> m a
runLogFun ToStderr = Logging.runStderrLoggingT
runLogFun (ToFile path) =
  error ("logging to file (" ++ path ++ ") not implemented yet")

logExceptions :: Logger -> IO () -> IO ()
logExceptions logger action =
  action `catch`
  (withLogging logger . Logging.logErrorN . convertString . show @SomeException)

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Hanabi.Logging
  ( startLogging
  , logToStderr
  , logToFile
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
  => LogFunction -> (Logger -> LoggingT IO ()) -> m ()
startLogging logFun action = do
  chan <- liftIO Chan.newChan
  logThread <- liftIO (async (runLogFun logFun (Logging.unChanLoggingT chan)))
  let logger = (Logger logThread chan)
  result <- logExceptions logger (withLogging logger ((action logger)))
  liftIO (cancel logThread)
  return result

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

logExceptions
  :: MonadIO m
  => Logger -> IO () -> m ()
logExceptions logger action =
  liftIO
    (action `catch`
     (withLogging logger .
      Logging.logErrorN . convertString . show @SomeException))

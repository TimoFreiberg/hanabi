{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Hanabi.Logging
  ( startLogging
  , stopLogging
  , logToStderr
  , logToFile
  , logExceptions
  , LogChan
  ) where

import Control.Concurrent.Async (async, cancel, Async)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, putMVar, tryReadMVar)
import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Logger as Logger
import Control.Monad.Logger (LoggingT)
import Data.String.Conversions (convertString)
import System.IO.Unsafe (unsafePerformIO)

logThread :: MVar (Async (LoggingT IO ()))
logThread = unsafePerformIO newEmptyMVar

startLogging :: LogFunction -> IO LogChan
startLogging logFun = do
  chan <- Chan.newChan
  --FIXME what to do with the logging channel?
  logger <- (async . (runLogFun logFun) . Logger.unChanLoggingT) chan
  putMVar logThread logger
  return chan

stopLogging :: IO ()
stopLogging =
  tryReadMVar logThread >>= \case
    Nothing -> return ()
    Just thread -> cancel thread

type LogChan = Chan (Logger.Loc, Logger.LogSource, Logger.LogLevel, Logger.LogStr)

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
runLogFun ToStderr = Logger.runStderrLoggingT
runLogFun (ToFile path) =
  error ("logging to file (" ++ path ++ ") not implemented yet")

logExceptions :: LogChan -> IO () -> IO ()
logExceptions logChan action =
  action `catch`
  (Logger.runChanLoggingT logChan .
   Logger.logErrorN . convertString . show @SomeException)

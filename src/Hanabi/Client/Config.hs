{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hanabi.Client.Config where

import Control.Applicative (empty, Alternative)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Logger as Logger
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import Data.Monoid ((<>))
import Data.String.Conversions (convertString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as IO
import qualified System.Directory as Dir
import Text.Read (readMaybe)

import Hanabi.Client.Cli (ask)

getConfig :: LoggingT IO (Text, Int)
getConfig = do
  config <- runMaybeT readConfig
  (host, port) <- lift (maybe askConfig return config)
  Logger.logInfoN
    ("Set host (" <> host <> ") and port (" <> convertString (show port) <> ")")
  return (host, port)

askConfig :: IO (Text, Int)
askConfig = do
  ip <- ask "Enter IP:"
  port <-
    untilSuccess
      "Port must be a number"
      ((readMaybe . Text.unpack) <$> ask "Enter port:")
  return (ip, port)

readConfig :: MaybeT (LoggingT IO) (Text, Int)
readConfig = do
  currentDir <- (<> "/") <$> liftIO Dir.getCurrentDirectory
  let configFileName = "hanabi.config"
  Logger.logDebugN
    ("Trying to read config from " <> convertString currentDir <>
     convertString configFileName)
  configExists <- liftIO (Dir.doesFileExist configFileName)
  guardLog "Config file not found." configExists
  config <- liftIO (fmap Text.lines (IO.readFile configFileName))
  guardLog "Wrong format of config file" (length (take 2 config) == 2)
  let [ip, portString] = config
  port <- MaybeT ((return . readMaybe . Text.unpack) portString)
  return (ip, port)

guardLog
  :: (Logger.MonadLogger f, Alternative f)
  => Text -> Bool -> f ()
guardLog _ True = pure ()
guardLog msg False = Logger.logDebugN msg >> empty

untilSuccess :: Text -> IO (Maybe a) -> IO a
untilSuccess msg action =
  action >>= \case
    Just x -> return x
    Nothing -> IO.putStrLn msg >> untilSuccess msg action

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Hanabi.Client
  ( startClient
  ) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (encode, eitherDecode, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.List.NonEmpty as List
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (convertString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.WebSockets as WS
import qualified System.IO as Handle

import qualified Hanabi
import qualified Hanabi.Client.Cli as Cli
import Hanabi.Client.Config (getConfig)
import qualified Hanabi.Client.Messaging as Msg
import Hanabi.Logging
       (startLogging, withLogging, logToStderr, Logger, logInfo, logDebug,
        logWarn)
import qualified Hanabi.Print as Print

send
  :: (MonadLogger m, MonadIO m, ToJSON a)
  => WS.Connection -> a -> m ()
send conn x = do
  let msg = encode x
  logInfo ("Sending JSON:\n" <> convertString (encodePretty x))
  liftIO (WS.sendTextData conn msg)

receive :: WS.Connection -> LoggingT IO (Either String Msg.Response)
receive c = do
  resp <- liftIO (WS.receiveData c)
  logDebug ("Received JSON:\n" <> convertString resp)
  return (eitherDecode resp)

startClient :: IO ()
startClient = do
  Handle.hSetBuffering Handle.stdin Handle.LineBuffering
  Handle.hSetBuffering Handle.stdout Handle.LineBuffering
  Handle.hSetBuffering Handle.stderr Handle.LineBuffering
  startLogging logToStderr $ \logger -> do
    logDebug "Starting client."
    (host, port) <- getConfig
    name <- Cli.ask "Enter name:"
    logDebug ("Set name (" <> name <> ")")
    lift
      (WS.runClient
         (convertString host)
         port
         "/"
         (withLogging logger . client logger name))
    logDebug "Client stopped."

client :: Logger -> Text -> WS.Connection -> LoggingT IO ()
client logger myName conn = do
  gameStore <- liftIO newEmptyMVar
  gameEnded <- liftIO (newEmptyMVar @())
  receiveThread <-
    liftIO
      (async (withLogging logger (receiver myName gameStore gameEnded conn)))
  send conn (Msg.ConnectionRequest myName)
  let myId = Hanabi.PlayerId myName
  let inputHandler = do
        input <- Text.toLower <$> Cli.getLn
        response <- Cli.inputHandler gameStore myId input
        case response of
          Cli.SendRequest r -> send conn r
          Cli.InputErr e -> Cli.putLn e
  let loop = do
        inputHandler
        liftIO (tryTakeMVar gameEnded) >>= \case
          Nothing -> loop
          Just _ -> return ()
  loop
  liftIO (cancel receiveThread)

receiver
  :: Text
  -> MVar (List.NonEmpty Hanabi.Game)
  -> MVar ()
  -> WS.Connection
  -> LoggingT IO ()
receiver myName gameStore gameEnded conn =
  receive conn >>= \case
    Right response ->
      case response of
        Msg.GameOverResponse finalScore -> do
          liftIO (putMVar gameEnded ())
          Cli.putLn ("game over - score: " <> Cli.showT finalScore)
        Msg.ErrorResponse expl details -> do
          logWarn
            ("received error message:\n" <> fromMaybe "" details <> "\n" <> expl)
          loop
        Msg.ConnectionResponse playerNames -> do
          Cli.putLn ("current players: " <> Text.intercalate ", " playerNames) --FIXME
          loop
        resp -> do
          let game = Msg.toHanabi (Msg.game_state resp)
          liftIO
            (tryTakeMVar gameStore >>=
             (putMVar gameStore . (maybe (game List.:| [])) (game List.<|)))
          Print.selectiveFairPrint (Hanabi.PlayerId myName) game
          loop
    Left parseError -> logWarn (convertString parseError) >> loop
  where
    loop = receiver myName gameStore gameEnded conn

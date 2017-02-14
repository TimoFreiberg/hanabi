{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Hanabi.Client
  ( startClient
  ) where

import Control.Concurrent.Async (async, cancel)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, putMVar, tryTakeMVar, readMVar)
import Control.Lens (view, at, to, non)
import Control.Monad (guard, (>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Control.Monad.Logger as Logger
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (encode, eitherDecode, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.List.NonEmpty as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (convertString, ConvertibleStrings)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.WebSockets as WS
import qualified System.IO as Handle
import Text.Read (readMaybe)

import qualified Hanabi
import qualified Hanabi.Client.Cli as Cli
import Hanabi.Client.Config (getConfig)
import qualified Hanabi.Client.Messaging as Msg
import Hanabi.Logging
       (startLogging, stopLogging, logToStderr, logExceptions)
import qualified Hanabi.Print as Print

send
  :: (MonadLogger m, MonadIO m, ToJSON a)
  => WS.Connection -> a -> m ()
send conn x = do
  let msg = encode x
  Logger.logInfoN ("Sending JSON:\n" <> convertString (encodePretty x))
  liftIO (WS.sendTextData conn msg)

receive :: WS.Connection -> LoggingT IO (Either String Msg.Response)
receive c = do
  resp <- liftIO (WS.receiveData c)
  Logger.logDebugN ("Received JSON:\n" <> convertString resp)
  return (eitherDecode resp)

startClient :: IO ()
startClient = do
  Handle.hSetBuffering Handle.stdin Handle.LineBuffering
  Handle.hSetBuffering Handle.stdout Handle.LineBuffering
  Handle.hSetBuffering Handle.stderr Handle.LineBuffering
  logChan <- startLogging logToStderr
  logExceptions
    logChan
    (Logger.runChanLoggingT logChan $ do
       Logger.logDebugN "Starting client."
       (host, port) <- getConfig
       name <- Cli.ask "Enter name:"
       Logger.logDebugN ("Set name (" <> name <> ")")
       lift
         (WS.runClient
            (convertString host)
            port
            "/"
            (Logger.runChanLoggingT logChan . client logChan name))
       Logger.logDebugN "Client stopped.")
  stopLogging

client
  :: Chan.Chan (Logger.Loc, Logger.LogSource, Logger.LogLevel, Logger.LogStr)
  -> Text
  -> WS.Connection
  -> LoggingT IO ()
client logChan myName conn = do
  gameStore <- liftIO newEmptyMVar
  gameEnded <- liftIO (newEmptyMVar @())
  receiveThread <-
    liftIO
      (async
         (Logger.runChanLoggingT
            logChan
            (receiver myName gameStore gameEnded conn)))
  send conn (Msg.ConnectionRequest myName)
  let myId = Hanabi.PlayerId myName
  let inputHandler = do
        input <- Text.toLower <$> Cli.getLn
        if (input == "start")
          -- FIXME: dirty hack.
          -- necessary because no games are stored before the game is started
          then (send conn Msg.GameStartRequest)
          else do
            game <- liftIO (List.head <$> readMVar gameStore)
            case input of
              (checkPlay game myId -> Just playWhat) -> do
                (send
                   conn
                   (Msg.PlayCardRequest (getCardIdAt game myId playWhat)))
              (checkDiscard game myId -> Just discardWhat) -> do
                (send
                   conn
                   (Msg.DiscardCardRequest (getCardIdAt game myId discardWhat)))
              (checkHint game myId >=> Cli.extractColorHint -> Just (hintWhom, hintColor)) -> do
                send conn (Msg.HintColorRequest hintWhom hintColor)
              (checkHint game myId >=> Cli.extractNumberHint -> Just (hintWhom, hintNumber)) -> do
                send conn (Msg.HintNumberRequest hintWhom hintNumber)
              _ -> Cli.putLn "couldn't read input"
  let loop = do
        inputHandler
        liftIO (tryTakeMVar gameEnded) >>= \case
          Nothing -> loop
          Just _ -> return ()
  loop
  liftIO (cancel receiveThread)

checkDiscard :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe Int
checkDiscard game name input = do
  discardWhat <- Text.stripPrefix "discard" input
  i <- readT discardWhat
  checkCardIndex game name i

checkPlay :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe Int
checkPlay game name input = do
  playWhat <- Text.stripPrefix "play" input
  i <- readT playWhat
  checkCardIndex game name i

checkHint :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe (Text, Text)
checkHint game name input = do
  params <- Text.stripPrefix "hint" input
  let tokens = Text.words params
  guard (length tokens >= 2)
  let hintWhom = Text.concat (init tokens)
  let hintWhat = last tokens
  guard (hintWhom /= (convertString name))
  guard
    ((Hanabi.PlayerId hintWhom) `elem`
     (view (Hanabi.playerHands . to Map.keys) game))
  return (hintWhom, hintWhat)

checkCardIndex :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Maybe Int
checkCardIndex game name i
  | i >= 0 && i < handSize = Just i
  | otherwise = Nothing
  where
    handSize = length (myHand game name)

readT
  :: (ConvertibleStrings a String, Read b)
  => a -> Maybe b
readT = readMaybe . convertString

showT
  :: (Show a, ConvertibleStrings String b)
  => a -> b
showT = convertString . show

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
          Cli.putLn ("game over - score: " <> showT finalScore)
        Msg.ErrorResponse expl details -> do
          Logger.logWarnN
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
    Left parseError -> Logger.logWarnN (convertString parseError) >> loop
  where
    loop = receiver myName gameStore gameEnded conn

getCardIdAt :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Int
getCardIdAt g n i = view (Hanabi.cardId) (fst ((myHand g n) !! i))

myHand :: Hanabi.Game -> Hanabi.PlayerId -> Hanabi.Hand
myHand game name = view (Hanabi.playerHands . at name . non []) game

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Hanabi.Client where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, takeMVar, putMVar, withMVar, tryTakeMVar,
        newMVar, readMVar, modifyMVar_)
import Control.Exception (catch, SomeException)
import Control.Lens (view, at, to, non)
import Control.Monad (guard, when, (>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Control.Monad.Logger as Logger
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import Data.Aeson (encode, decode, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Foldable (asum)
import Data.IORef
       (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.List.NonEmpty as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (convertString, ConvertibleStrings)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as IO
import qualified GHC.Base (empty)
import qualified Network.WebSockets as WS
import qualified System.Directory as Dir
import qualified System.IO as Handle
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.EscapeArtist as IO
import Text.Read (readMaybe)

import qualified Hanabi
import Hanabi.Client.Cli
import Hanabi.Client.Messaging
import qualified Hanabi.Print as Print

port :: Int
port = 4444

host :: String
host = "localhost"

path :: String
path = "/"

send conn x = do
  let msg = encode x
  Logger.logInfoN ("Sending JSON:\n" <> convertString (encodePretty x))
  liftIO (WS.sendTextData conn msg)

receive :: WS.Connection -> LoggingT IO (Either String Response)
receive c = do
  resp <- liftIO (WS.receiveData c)
  Logger.logDebugN ("received:\n" <> convertString resp)
  return (eitherDecode resp)

startClient = do
  Handle.hSetBuffering Handle.stdin Handle.LineBuffering
  Handle.hSetBuffering Handle.stdout Handle.LineBuffering
  Handle.hSetBuffering Handle.stderr Handle.LineBuffering
  logChan <- Chan.newChan
  logThread <-
    (async . Logger.runStderrLoggingT . Logger.unChanLoggingT) logChan
  flip
    catch
    (Logger.runChanLoggingT logChan .
     Logger.logErrorN . convertString . show @SomeException)
    (Logger.runChanLoggingT logChan $ do
       Logger.logDebugN "Starting client."
       (host, port) <- getConfig
       name <- ask "Enter name:"
       Logger.logDebugN ("Set name (" <> name <> ")")
       lift
         (WS.runClient
            (convertString host)
            port
            "/"
            (Logger.runChanLoggingT logChan . client logChan name))
       Logger.logDebugN "Client stopped.")

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

untilSuccess msg action =
  action >>= \case
    Just x -> return x
    Nothing -> IO.putStrLn msg >> untilSuccess msg action

ask msg =
  liftIO $ do
    IO.putStrLn msg
    IO.getLine

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

guardLog _ True = pure ()
guardLog msg False = Logger.logDebugN msg >> GHC.Base.empty

client logChan myName conn = do
  gameStore <- liftIO newEmptyMVar
  gameEnded <- liftIO (newEmptyMVar @())
  receiveThread <-
    liftIO
      (async
         (Logger.runChanLoggingT
            logChan
            (receiver myName gameStore gameEnded conn)))
  send conn (ConnectionRequest myName)
  let myId = Hanabi.PlayerId myName
  let inputHandler = do
        input <- Text.toLower <$> getLn
        if (input == "start")
          -- FIXME: dirty hack.
          -- necessary because no games are stored before the game is started
          then (send conn GameStartRequest)
          else do
            game <- liftIO (List.head <$> readMVar gameStore)
            case input of
              (checkPlay game myId -> Just playWhat) -> do
                (send conn (PlayCardRequest (getCardAt game myId playWhat)))
              (checkDiscard game myId -> Just discardWhat) -> do
                (send
                   conn
                   (DiscardCardRequest (getCardAt game myId discardWhat)))
              (checkHint game myId >=> extractColorHint -> Just (hintWhom, hintColor)) -> do
                send conn (HintColorRequest hintWhom hintColor)
              (checkHint game myId >=> extractNumberHint -> Just (hintWhom, hintNumber)) -> do
                send conn (HintNumberRequest hintWhom hintNumber)
              _ -> putLn "couldn't read input"
  let loop = do
        inputHandler
        liftIO (tryTakeMVar gameEnded) >>= \case
          Nothing -> loop
          Just _ -> return ()
  loop

checkDiscard game name input = do
  discardWhat <- Text.stripPrefix "discard" input
  i <- readT discardWhat
  checkCardIndex game name i

checkPlay :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe Int
checkPlay game name input = do
  playWhat <- Text.stripPrefix "play" input
  i <- readT playWhat
  checkCardIndex game name i

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

getLn
  :: MonadIO m
  => m Text
getLn = liftIO IO.getLine

readT
  :: (ConvertibleStrings a String, Read b)
  => a -> Maybe b
readT = readMaybe . convertString

showT
  :: (Show a, ConvertibleStrings String b)
  => a -> b
showT = convertString . show

putLn
  :: (MonadIO m)
  => Text -> m ()
putLn = liftIO . IO.putStrLn . convertString

receiver myName gameStore gameEnded conn =
  receive conn >>= \case
    Right response ->
      case response of
        GameOverResponse finalScore -> do
          liftIO (putMVar gameEnded ())
          putLn ("game over - score: " <> showT finalScore)
        ErrorResponse expl details -> do
          Logger.logWarnN
            ("received error message:\n" <> fromMaybe "" details <> "\n" <> expl)
          loop
        ConnectionResponse playerNames -> do
          putLn ("current players: " <> Text.intercalate ", " playerNames) --FIXME
          loop
        resp -> do
          let game = toHanabi (game_state resp)
          liftIO
            (tryTakeMVar gameStore >>=
             (putMVar gameStore . (maybe (game List.:| [])) (game List.<|)))
          Print.selectiveFairPrint (Hanabi.PlayerId myName) game
          loop
    Left parseError -> Logger.logWarnN (convertString parseError) >> loop
  where
    loop = receiver myName gameStore gameEnded conn

getCardAt :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Card
getCardAt g n i = fromCard (fst ((myHand g n) !! i))

myHand :: Hanabi.Game -> Hanabi.PlayerId -> Hanabi.Hand
myHand game name = view (Hanabi.playerHands . at name . non []) game

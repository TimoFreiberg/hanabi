{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Hanabi.Client where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, takeMVar, putMVar, withMVar)
import Control.Lens (view, at, to, non)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Logger as Logger
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Foldable (asum)
import Data.IORef
       (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
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
import Hanabi.Client.Messaging
import Hanabi.Print as Print

port :: Int
port = 4444

host :: String
host = "localhost"

path :: String
path = "/"

{-# NOINLINE connRef #-}
connRef :: MVar WS.Connection
connRef = unsafePerformIO $ newEmptyMVar

{-# NOINLINE playerName #-}
playerName :: IORef Text
playerName = unsafePerformIO $ newIORef (Text.empty)

{-# NOINLINE endGame #-}
endGame :: MVar ()
endGame = unsafePerformIO $ newEmptyMVar

{-# NOINLINE games #-}
games :: IORef [Hanabi.Game]
games = unsafePerformIO $ newIORef []

send :: Request -> IO ()
send x =
  withMVar
    connRef
    (\c -> do
       let msg = encode x
       ByteString.putStrLn "sending:"
       ByteString.putStrLn (msg)
       WS.sendTextData c msg)

receive :: WS.Connection -> IO (Either String Response)
receive c = do
  resp <- WS.receiveData c
  ByteString.putStrLn "received:"
  ByteString.putStrLn resp
  return (eitherDecode resp)

go :: Text -> IO (Async ())
go myName = async (WS.runClient host port path (client myName))

startClient =
  Logger.runStderrLoggingT $ do
    Logger.logDebugN "Starting client."
    liftIO (Handle.hSetBuffering Handle.stdin Handle.NoBuffering)
    config <- runMaybeT readConfig
    (host, port) <- lift (maybe askConfig return config)
    Logger.logInfoN
      ("Set host (" <> host <> ") and port (" <> Text.pack (show port) <> ")")

askConfig :: IO (Text, Int)
askConfig = do
  ip <- ask "Enter IP:"
  port <-
    untilSuccess
      "Port must be a number with four digits"
      ((readMaybe @Int . Text.unpack) <$> ask "Enter port:")
  return (ip, port)

untilSuccess msg action =
  action >>= \case
    Just x -> return x
    Nothing -> IO.putStrLn msg >> untilSuccess msg action

ask msg = do
  IO.putStr msg
  IO.getLine

readConfig :: MaybeT (LoggingT IO) (Text, Int)
readConfig = do
  currentDir <- liftIO Dir.getCurrentDirectory
  let configFileName = "hanabi.config"
  Logger.logDebugN
    ("Trying to read config from " <> Text.pack currentDir <>
     Text.pack configFileName)
  configExists <- liftIO (Dir.doesFileExist configFileName)
  guardLog "Config file not found." configExists
  config <- liftIO (fmap Text.lines (IO.readFile configFileName))
  guardLog "Wrong format of config file" (length (take 2 config) == 2)
  let [ip, portString] = config
  port <- MaybeT ((return . readMaybe @Int . Text.unpack) portString)
  return (ip, port)

guardLog _ True = pure ()
guardLog msg False = Logger.logDebugN msg >> GHC.Base.empty

client :: Text -> WS.Connection -> IO ()
client myName conn = do
  writeIORef playerName myName
  putMVar connRef conn
  async (receiver myName conn)
  send (ConnectionRequest myName)
  takeMVar endGame

receiver :: Text -> WS.Connection -> IO ()
receiver myName conn =
  receive conn >>= \case
    Right response ->
      case response of
        GameOverResponse finalScore ->
          putMVar endGame () >>
          putStrLn ("game over - score: " ++ show finalScore)
        ErrorResponse expl details -> print details >> print expl >> loop
        ConnectionResponse playerNames ->
          putStrLn ("current players: " ++ show playerNames) >> loop
        resp -> do
          let game = toHanabi (game_state resp)
          modifyIORef' games (game :)
          Print.selectiveFairPrint (Hanabi.PlayerId myName) game
          loop
    Left parseError -> print parseError >> loop
  where
    loop = receiver myName conn

start :: IO ()
start = send GameStartRequest

play :: Int -> IO ()
play i = do
  g <- fmap head (readIORef games)
  n <- fmap Hanabi.PlayerId (readIORef playerName)
  send (PlayCardRequest (fromCard (getCardAt g n i)))

hintC :: Text -> Color -> IO ()
hintC target col = send (HintColorRequest target col)

hintN :: Text -> Number -> IO ()
hintN target num = send (HintNumberRequest target num)

discard :: Int -> IO ()
discard i = do
  g <- fmap head (readIORef games)
  n <- fmap Hanabi.PlayerId (readIORef playerName)
  send (DiscardCardRequest (fromCard (getCardAt g n i)))

getCardAt :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Hanabi.Card
getCardAt g n i =
  view (Hanabi.playerHands . at n . non [] . to (fst . (!! i))) g

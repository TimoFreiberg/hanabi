{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hanabi.Client where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, takeMVar, putMVar, withMVar)
import Control.Lens (view, at, to, ix, non)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Logger as Logger
import Data.Aeson (encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.IORef
       (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.WebSockets as WS
import System.IO.Unsafe (unsafePerformIO)

import qualified Hanabi
import Hanabi.Client.Messaging
import Hanabi.Print as Print
import qualified Hanabi.Repl as Repl

port = 4444

host = "localhost"

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

go name = async (startClient name)

client name conn = do
  writeIORef playerName name
  putMVar connRef conn
  async (receiver name conn)
  send (ConnectionRequest name)
  takeMVar endGame

receiver name conn =
  receive conn >>= \case
    Right response ->
      case response of
        GameOverResponse score ->
          putMVar endGame () >> putStrLn ("game over - score: " ++ show score)
        ErrorResponse expl details -> print details >> print expl >> loop
        ConnectionResponse names ->
          putStrLn ("current players: " ++ show names) >> loop
        resp -> do
          let game = toHanabi (game_state resp)
          modifyIORef' games (game :)
          Print.selectiveFairPrint (Hanabi.PlayerId name) game
          loop
    Left parseError -> print parseError >> loop
  where
    loop = receiver name conn

startClient name = WS.runClient host port path (client name)

start = send GameStartRequest

play i = do
  g <- fmap head (readIORef games)
  n <- fmap Hanabi.PlayerId (readIORef playerName)
  send (PlayCardRequest (fromCard (getCardAt g n i)))

hintC to col = send (HintColorRequest to col)

hintN to num = send (HintNumberRequest to num)

discard i = do
  g <- fmap head (readIORef games)
  n <- fmap Hanabi.PlayerId (readIORef playerName)
  send (DiscardCardRequest (fromCard (getCardAt g n i)))

getCardAt g n i =
  view (Hanabi.playerHands . at n . non [] . to (fst . (!! i))) g

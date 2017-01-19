{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Client where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Logger as Logger
import Data.Aeson (encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.WebSockets as WS
import System.IO.Unsafe (unsafePerformIO)

import Hanabi.Client.Messaging
import Hanabi.Repl

port = 4444

host = "localhost"

path = "/"

{-# NOINLINE connRef #-}
connRef :: MVar WS.Connection
connRef = unsafePerformIO $ newEmptyMVar

getConn = takeMVar connRef

{-# NOINLINE endGame #-}
endGame :: MVar ()
endGame = unsafePerformIO $ newEmptyMVar

send c x = WS.sendTextData c (encode x)

receive c = WS.receiveData c

exchange :: WS.Connection -> Request -> IO (Either String Response)
exchange c x = send c x >> (fmap eitherDecode (receive c))

go = async startClient

startClient =
  WS.runClient
    host
    port
    path
    (\conn -> do
       putMVar connRef conn
       takeMVar endGame)

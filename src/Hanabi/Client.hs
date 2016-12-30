{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Client where

import Hanabi.Messages

import qualified Network.WebSockets as WS
import qualified Data.Text as Text
import qualified Control.Monad.Logger as Logger
import Data.Aeson (decode, encode)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)

runClient :: (Event -> IO (Maybe Action)) -> WS.Connection -> IO ()
runClient eventHandler conn =
  Logger.runStdoutLoggingT $
  do let loop :: Logger.LoggingT IO ()
         loop = do
           rawMsg <- liftIO (WS.receiveData conn)
           $(Logger.logInfoSH) ("Receiving " <> rawMsg)
           action <-
             case decode rawMsg of
               Nothing ->
                 $(Logger.logWarnSH) ("Could not parse " <> rawMsg) >> return Nothing
               Just msg -> liftIO (eventHandler msg)
           case action of
             Nothing -> return () --FIXME when does loop stop? 
             Just answer -> do
               let rawAnswer = encode answer
               $(Logger.logInfoSH) ("Sending " <> rawAnswer)
               liftIO (WS.sendTextData conn rawAnswer)
               loop
     loop
     liftIO (WS.sendClose conn (Text.empty))

runAction :: Event -> IO Bool
runAction (CardPlayed playerId card) = undefined >> return True
runAction (CardDiscarded playerId card) = undefined >> return True
runAction (HintGained) = undefined >> return True
runAction (HintSpent) = undefined >> return True
runAction (FuckedUp) = undefined >> return True
runAction (GameOver score) = undefined >> return False
runAction (HintGiven playerId hint) = undefined >> return True
runAction (CardDrawn playerId card) = undefined >> return True
runAction (PlayerTurn playerId) = undefined >> return True

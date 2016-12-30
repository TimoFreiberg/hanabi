{-# LANGUAGE DeriveGeneric #-}

module Hanabi.Client.Messaging where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Hanabi

data Request
  = ConnectionRequest PlayerId
  | DiscardCardRequest Card
  | HintColorRequest PlayerId
                     Color
  | HintNumberRequest PlayerId
                      Number
  | PlayCardRequest Card
  | GameStartRequest
  deriving (Show, Generic)

data Response
  = ErrorResponse Text
                  (Maybe Text)
  | ConnectionResponse PlayerId
  | DiscardCardResponse GameUpdate
  | PlayCardResponse GameUpdate
  | HintColorResponse GameUpdate
  | HintNumberResponse GameUpdate
  | GameOverResponse Score
  | GameStartResponse
  deriving (Show, Generic)

instance FromJSON Response

instance ToJSON Response

data GameUpdate =
  GameUpdate PlayerId
             Game
  deriving (Show, Generic)

instance FromJSON GameUpdate

instance ToJSON GameUpdate

newtype Score =
  Score Int
  deriving (Show, Generic)

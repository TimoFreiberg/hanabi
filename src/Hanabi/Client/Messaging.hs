{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Client.Messaging where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Control.Lens (view, to, filtered)

import Hanabi
import qualified Hanabi.Game as Game
import Hanabi.Transitions
import Hanabi.Print
import Hanabi.Repl

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
  | DiscardCardResponse Game
  | PlayCardResponse Game
  | HintColorResponse Game
  | HintNumberResponse Game
  | GameOverResponse Score
  | GameStartResponse
  deriving (Show, Generic)

newtype Score =
  Score Int
  deriving (Show, FromJSON, ToJSON)

instance FromJSON Response

instance ToJSON Response where
  toJSON msg = object (["msg_type" .= msgType] ++ msgPayload)
    where
      (msgType, msgPayload) = msgData msg
      msgData :: Response -> (Text, [(Text, Value)])
      msgData (ErrorResponse explanation details) =
        ( "ERROR_RESPONSE"
        , ["explanation" .= explanation, "err_details" .= details])
      msgData (ConnectionResponse name) =
        ("CONNECTION_RESPONSE", ["name" .= name])
      msgData (DiscardCardResponse game) =
        ("DISCARD_CARD_RESPONSE", gameState game)
      msgData (PlayCardResponse game) = ("PLAY_CARD_RESPONSE", gameState game)
      msgData (HintColorResponse game) = ("HINT_COLOR_RESPONSE", gameState game)
      msgData (HintNumberResponse game) =
        ("HINT_NUMBER_RESPONSE", gameState game)
      msgData (GameOverResponse score) =
        ("GAME_OVER_RESPONSE", ["score" .= score])
      msgData (GameStartResponse) = ("GAME_START_RESPONSE", mempty)

gameState game =
  [ "next_player" .= view activePlayer game
  , "game_state" .=
    object
      [ "hint_tokens" .= view hints game
      , "hint_tokens_max" .= Game.initialHints
      , "err_tokens" .= view fuckups game
      , "played_cards" .=
        view
          (playedCards .
           filtered (not . Map.null) . to (fmap ((view number) . head)))
          game
      , "players" .= view playerHands game // FIXME
      , "deck" .= view deck game
      ]
  ]

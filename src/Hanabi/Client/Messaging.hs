{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Client.Messaging where

import Control.Lens (view, to, filtered)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as ByteString
import Hanabi
import qualified Hanabi.Game as Game
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

gameState
  :: KeyValue t
  => Game -> [t]
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
      , "players" .= encodePlayers (view (playerHands) game)
      , "deck" .= view deck game
      ]
  ]

encodePlayers players =
  [ object ["name" .= name, "cards" .= encodeCardInHand hand]
  | (name, hand) <- Map.assocs players
  ]

encodeCardInHand hand =
  [ object ["card" .= card, "knowledge" .= encodeCardKnowledge facts]
  | (card, facts) <- hand
  ]

encodeCardKnowledge facts =
  object
    [ "knows_color" .= knowsColor
    , "knows_number" .= knowsNumber
    , "knows_color_not" .= Set.map toColor negativeColorFacts
    , "knows_number_not" .= Set.map toNumber negativeNumberFacts
    ]
  where
    (posFacts, negFacts) = Set.partition (Game.isPositiveFact) facts
    knowsColor = any Game.isColorFact posFacts
    knowsNumber = any Game.isNumberFact posFacts
    (negativeColorFacts, negativeNumberFacts) =
      Set.partition Game.isColorFact negFacts
    toColor (Not (IsColor c)) = c
    toColor x = error $ show x ++ " must be a negative color fact"
    toNumber (Not (IsNumber n)) = n
    toNumber x = error $ show x ++ " must be a negative number fact"

players = view playerHands game

testPlayers = test (encodePlayers players)

testCardKnowledge =
  test $ fmap (fmap (encodeCardKnowledge . snd)) $ Map.elems players

test x = ByteString.putStrLn (encodePretty x)

testGame = test (HintNumberResponse game)

game :: Game
game =
  fromJust . decode $
  "{\"activePlayer\":\"1\",\"playerHands\":{\"alice\":[[{\"color\":\"White\",\"number\":\"Two\"},[]],[{\"color\":\"Green\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"color\":\"Yellow\",\"number\":\"Five\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"color\":\"White\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"color\":\"Yellow\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]]],\"bob\":[[{\"color\":\"Red\",\"number\":\"Four\"},[]],[{\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Three\"}}]],[{\"color\":\"White\",\"number\":\"Three\"},[{\"tag\":\"IsNumber\",\"contents\":\"Three\"}]],[{\"color\":\"Yellow\",\"number\":\"Three\"},[{\"tag\":\"IsNumber\",\"contents\":\"Three\"}]],[{\"color\":\"Blue\",\"number\":\"Three\"},[{\"tag\":\"IsNumber\",\"contents\":\"Three\"}]]],\"charlie\":[[{\"color\":\"Green\",\"number\":\"One\"},[]],[{\"color\":\"Green\",\"number\":\"Five\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"color\":\"Yellow\",\"number\":\"One\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"IsNumber\",\"contents\":\"Two\"}]],[{\"color\":\"White\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]]]},\"deck\":[{\"color\":\"White\",\"number\":\"One\"},{\"color\":\"Blue\",\"number\":\"Four\"},{\"color\":\"Yellow\",\"number\":\"Four\"},{\"color\":\"Blue\",\"number\":\"One\"},{\"color\":\"Green\",\"number\":\"Two\"},{\"color\":\"Red\",\"number\":\"Three\"},{\"color\":\"Red\",\"number\":\"Five\"},{\"color\":\"Red\",\"number\":\"One\"},{\"color\":\"Blue\",\"number\":\"Four\"},{\"color\":\"White\",\"number\":\"One\"},{\"color\":\"Green\",\"number\":\"Three\"},{\"color\":\"Blue\",\"number\":\"Three\"},{\"color\":\"White\",\"number\":\"One\"},{\"color\":\"Yellow\",\"number\":\"Four\"},{\"color\":\"Green\",\"number\":\"One\"},{\"color\":\"White\",\"number\":\"Three\"},{\"color\":\"Yellow\",\"number\":\"One\"},{\"color\":\"White\",\"number\":\"Four\"},{\"color\":\"Green\",\"number\":\"One\"},{\"color\":\"Green\",\"number\":\"Three\"},{\"color\":\"Blue\",\"number\":\"One\"},{\"color\":\"Blue\",\"number\":\"Five\"},{\"color\":\"Red\",\"number\":\"Four\"},{\"color\":\"Green\",\"number\":\"Four\"},{\"color\":\"Red\",\"number\":\"One\"},{\"color\":\"White\",\"number\":\"Five\"},{\"color\":\"Green\",\"number\":\"Two\"},{\"color\":\"Blue\",\"number\":\"Two\"},{\"color\":\"Yellow\",\"number\":\"Two\"},{\"color\":\"Yellow\",\"number\":\"Three\"},{\"color\":\"Red\",\"number\":\"Three\"}],\"playedCards\":{\"Yellow\":[{\"color\":\"Yellow\",\"number\":\"One\"}],\"Blue\":[{\"color\":\"Blue\",\"number\":\"Two\"},{\"color\":\"Blue\",\"number\":\"One\"}],\"Red\":[{\"color\":\"Red\",\"number\":\"One\"}]},\"discardedCards\":[],\"hints\":3,\"fuckups\":0,\"lastPlayer\":null}"

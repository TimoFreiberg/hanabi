{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hanabi.Client.Messaging where

import Control.Applicative ((<|>))
import Control.Lens (view, to, filtered)
import Control.Monad (foldM)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
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

instance FromJSON Response where
  parseJSON (Object o) = do
    (msgType :: Text) <- o .: "msg_type"
    decodeResponse msgType
    where
      decodeResponse "ERROR_RESPONSE" =
        ErrorResponse <$> o .: "explanation" <*> o .: "err_details"
      decodeResponse "CONNECTION_RESPONSE" = ConnectionResponse <$> o .: "name"
      decodeResponse "DISCARD_CARD_RESPONSE" =
        DiscardCardResponse <$> decodeGameState o
      decodeResponse "PLAY_CARD_RESPONSE" =
        PlayCardResponse <$> decodeGameState o
      decodeResponse "HINT_COLOR_RESPONSE" =
        HintColorResponse <$> decodeGameState o
      decodeResponse "HINT_NUMBER_RESPONSE" =
        HintNumberResponse <$> decodeGameState o
      decodeResponse "GAME_OVER_RESPONSE" = GameOverResponse <$> o .: "score"
      decodeResponse "GAME_START_RESPONSE" = pure GameStartResponse
  parseJSON _ = mempty

decodeGameState o = do
  gameState <- o .: "game_state"
  Game <$> o .: "next_player" <*> decodePlayerHands gameState <*>
    decodeCards "deck" gameState <*>
    (decodePlayedCards gameState) <*>
    decodeCards "discarded_cards" gameState <*>
    gameState .: "hint_tokens" <*>
    gameState .: "err_tokens" <*>
    decodeLastPlayer o

decodeCards key o = do
  cards <- o .: key
  traverse decodeCard cards

decodeCard (Object o) = Card <$> o .: "id" <*> o .: "color" <*> o .: "number"
decodeCard _ = mempty

decodePlayerHands o = do
  (hands :: Array) <- o .: "players"
  foldM decodeHand Map.empty hands
  where
    decodeHand m (Object h) = do
      name <- h .: "name"
      cards <- h .: "cards"
      (hand :: Hand) <- traverse decodeCardInHand cards
      pure (Map.insert name hand m)
    decodeHand _ _ = mempty
    decodeCardInHand c = do
      card <- c .: "card"
      card' <- decodeCard card
      -- knowledge <- c .: "knowledge" --FIXME
      pure (card', Set.empty)

decodePlayedCards o = do
  played <- o .: "played_cards"
  let addStack m col = do
        maybeNum <- played .: (Text.pack (show col)) <|> (pure Nothing)
        case maybeNum of
          Just num ->
            pure (Map.insert col (fmap (Card (-1) col) [One .. num]) m)
          Nothing -> pure m
  foldM addStack Map.empty Game.colors

decodeLastPlayer = const (pure Nothing) --FIXME

instance ToJSON Response where
  toJSON msg = object (["msg_type" .= msgType] ++ msgPayload)
    where
      msgData :: Response -> (Text, [(Text, Value)])
      (msgType, msgPayload) = msgData msg
      msgData (ErrorResponse explanation details) =
        ( "ERROR_RESPONSE"
        , ["explanation" .= explanation, "err_details" .= details])
      msgData (ConnectionResponse name) =
        ("CONNECTION_RESPONSE", ["name" .= name])
      msgData (DiscardCardResponse game) =
        ("DISCARD_CARD_RESPONSE", encodeGameState game)
      msgData (PlayCardResponse game) =
        ("PLAY_CARD_RESPONSE", encodeGameState game)
      msgData (HintColorResponse game) =
        ("HINT_COLOR_RESPONSE", encodeGameState game)
      msgData (HintNumberResponse game) =
        ("HINT_NUMBER_RESPONSE", encodeGameState game)
      msgData (GameOverResponse score) =
        ("GAME_OVER_RESPONSE", ["score" .= score])
      msgData (GameStartResponse) = ("GAME_START_RESPONSE", mempty)

encodeGameState
  :: KeyValue t
  => Game -> [t]
encodeGameState game =
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
      , "deck" .= viewCards deck
      , "discarded_cards" .= viewCards discardedCards
      ]
  ]
  where
    viewCards l = view (l . to (fmap encodeCard)) game

encodeCard c =
  object
    ["id" .= view cardId c, "color" .= view color c, "number" .= view number c]

encodePlayers players =
  [ object ["name" .= name, "cards" .= encodeCardInHand hand]
  | (name, hand) <- Map.assocs players
  ]

encodeCardInHand hand =
  [ object ["card" .= encodeCard card, "knowledge" .= encodeCardKnowledge facts]
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

testDecode :: Maybe Response
testDecode = decode (encode (HintNumberResponse game))

test x = ByteString.putStrLn (encodePretty x)

testGame = test (HintNumberResponse game)

game :: Game
game =
  fromJust . decode $
  "{\"activePlayer\":\"Alice\",\"playerHands\":{\"Alice\":[[{\"cardId\":47,\"color\":\"Red\",\"number\":\"Four\"},[]],[{\"cardId\":26,\"color\":\"Green\",\"number\":\"Three\"},[]],[{\"cardId\":19,\"color\":\"Yellow\",\"number\":\"Five\"},[]],[{\"cardId\":8,\"color\":\"White\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"cardId\":24,\"color\":\"Green\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]]],\"Bob\":[[{\"cardId\":4,\"color\":\"White\",\"number\":\"Two\"},[]],[{\"cardId\":45,\"color\":\"Red\",\"number\":\"Three\"},[]],[{\"cardId\":28,\"color\":\"Green\",\"number\":\"Four\"},[]],[{\"cardId\":7,\"color\":\"White\",\"number\":\"Four\"},[]],[{\"cardId\":27,\"color\":\"Green\",\"number\":\"Four\"},[]]],\"Charlie\":[[{\"cardId\":38,\"color\":\"Blue\",\"number\":\"Four\"},[]],[{\"cardId\":35,\"color\":\"Blue\",\"number\":\"Three\"},[]],[{\"cardId\":42,\"color\":\"Red\",\"number\":\"One\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"cardId\":44,\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"IsNumber\",\"contents\":\"Two\"},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}}]],[{\"cardId\":17,\"color\":\"Yellow\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]]]},\"deck\":[{\"cardId\":10,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":40,\"color\":\"Red\",\"number\":\"One\"},{\"cardId\":5,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":29,\"color\":\"Green\",\"number\":\"Five\"},{\"cardId\":18,\"color\":\"Yellow\",\"number\":\"Four\"},{\"cardId\":25,\"color\":\"Green\",\"number\":\"Three\"},{\"cardId\":22,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":9,\"color\":\"White\",\"number\":\"Five\"},{\"cardId\":21,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":49,\"color\":\"Red\",\"number\":\"Five\"},{\"cardId\":20,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":37,\"color\":\"Blue\",\"number\":\"Four\"},{\"cardId\":36,\"color\":\"Blue\",\"number\":\"Three\"},{\"cardId\":15,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":31,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":0,\"color\":\"White\",\"number\":\"One\"},{\"cardId\":13,\"color\":\"Yellow\",\"number\":\"Two\"},{\"cardId\":46,\"color\":\"Red\",\"number\":\"Three\"},{\"cardId\":3,\"color\":\"White\",\"number\":\"Two\"},{\"cardId\":6,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":39,\"color\":\"Blue\",\"number\":\"Five\"},{\"cardId\":16,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":34,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":48,\"color\":\"Red\",\"number\":\"Four\"},{\"cardId\":23,\"color\":\"Green\",\"number\":\"Two\"},{\"cardId\":12,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":14,\"color\":\"Yellow\",\"number\":\"Two\"}],\"playedCards\":{\"White\":[{\"cardId\":2,\"color\":\"White\",\"number\":\"One\"}],\"Yellow\":[{\"cardId\":11,\"color\":\"Yellow\",\"number\":\"One\"}],\"Blue\":[{\"cardId\":33,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":32,\"color\":\"Blue\",\"number\":\"One\"}],\"Red\":[{\"cardId\":43,\"color\":\"Red\",\"number\":\"Two\"},{\"cardId\":41,\"color\":\"Red\",\"number\":\"One\"}]},\"discardedCards\":[{\"cardId\":30,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":1,\"color\":\"White\",\"number\":\"One\"}],\"hints\":5,\"fuckups\":1,\"lastPlayer\":null}"

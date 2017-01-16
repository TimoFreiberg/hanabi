{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Prelude hiding (id)

import qualified Data.ByteString.Lazy as ByteString
import qualified Hanabi as Hanabi
import qualified Hanabi.Game as Game
import Hanabi.Repl

data Request
  = ConnectionRequest Hanabi.PlayerId
  | DiscardCardRequest Card
  | HintColorRequest Hanabi.PlayerId
                     Hanabi.Color
  | HintNumberRequest Hanabi.PlayerId
                      Hanabi.Number
  | PlayCardRequest Card
  | GameStartRequest
  deriving (Show, Generic, FromJSON, ToJSON)

data Response
  = ErrorResponse Text
                  (Maybe Text)
  | ConnectionResponse Hanabi.PlayerId
  | DiscardCardResponse { next_player :: Text
                       ,  game_state :: GameState
                       ,  turns_left :: Maybe Int}
  | PlayCardResponse { next_player :: Text
                    ,  game_state :: GameState
                    ,  turns_left :: Maybe Int}
  | HintColorResponse { next_player :: Text
                     ,  game_state :: GameState
                     ,  turns_left :: Maybe Int}
  | HintNumberResponse { next_player :: Text
                      ,  game_state :: GameState
                      ,  turns_left :: Maybe Int}
  | GameOverResponse Score
  | GameStartResponse
  deriving (Show, Generic, FromJSON, ToJSON)

newtype Score =
  Score Int
  deriving (Show, Generic, FromJSON, ToJSON)

data GameState = GameState
  { hint_tokens :: Int
  , hint_tokens_max :: Int
  , err_tokens :: Int
  , played_cards :: Map Hanabi.Color Hanabi.Number
  , players :: [Player]
  , deck :: [Card]
  , discarded_cards :: [Card]
  } deriving (Show, Generic, FromJSON, ToJSON)

data Player = Player
  { name :: Text
  , cards :: [CardInHand]
  } deriving (Show, Generic, FromJSON, ToJSON)

data CardInHand = CardInHand
  { card :: Card
  , knowledge :: CardKnowledge
  } deriving (Show, Generic, FromJSON, ToJSON)

data CardKnowledge = CardKnowledge
  { knows_color :: Bool
  , knows_number :: Bool
  , knows_color_not :: Set Hanabi.Color
  , knows_number_not :: Set Hanabi.Number
  } deriving (Show, Generic, FromJSON, ToJSON)

data Card = Card
  { id :: Int
  , color :: Hanabi.Color
  , number :: Hanabi.Number
  } deriving (Show, Generic, FromJSON, ToJSON)

toHanabi :: Hanabi.PlayerId -> GameState -> Maybe Int -> Hanabi.Game
toHanabi playerId (GameState hints maxHints errs played players deck discardedCards) turnsLeft =
  Hanabi.Game playerId mkHands mkDeck mkPlayed mkDiscarded hints errs undefined
  where
    mkDeck = undefined
    mkDiscarded = undefined
    mkHands = undefined
    mkPlayed = undefined

-- testDecode = eitherDecode (encode (HintNumberResponse game))
test x = ByteString.putStrLn (encodePretty x)

-- testGame = test (HintNumberResponse "Alice" (GameState 5 8 1 (Map.fromList [])))
fromRight (Right x) = x
fromRight (Left x) = error $ show x

game :: Hanabi.Game
game =
  fromRight . eitherDecode $
  "{\"activePlayer\":\"Alice\",\"playerHands\":{\"Alice\":[[{\"cardId\":47,\"color\":\"Red\",\"number\":\"Four\"},[]],[{\"cardId\":26,\"color\":\"Green\",\"number\":\"Three\"},[]],[{\"cardId\":19,\"color\":\"Yellow\",\"number\":\"Five\"},[]],[{\"cardId\":8,\"color\":\"White\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"cardId\":24,\"color\":\"Green\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]]],\"Bob\":[[{\"cardId\":4,\"color\":\"White\",\"number\":\"Two\"},[]],[{\"cardId\":45,\"color\":\"Red\",\"number\":\"Three\"},[]],[{\"cardId\":28,\"color\":\"Green\",\"number\":\"Four\"},[]],[{\"cardId\":7,\"color\":\"White\",\"number\":\"Four\"},[]],[{\"cardId\":27,\"color\":\"Green\",\"number\":\"Four\"},[]]],\"Charlie\":[[{\"cardId\":38,\"color\":\"Blue\",\"number\":\"Four\"},[]],[{\"cardId\":35,\"color\":\"Blue\",\"number\":\"Three\"},[]],[{\"cardId\":42,\"color\":\"Red\",\"number\":\"One\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"cardId\":44,\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"IsNumber\",\"contents\":\"Two\"},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}}]],[{\"cardId\":17,\"color\":\"Yellow\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]]]},\"deck\":[{\"cardId\":10,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":40,\"color\":\"Red\",\"number\":\"One\"},{\"cardId\":5,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":29,\"color\":\"Green\",\"number\":\"Five\"},{\"cardId\":18,\"color\":\"Yellow\",\"number\":\"Four\"},{\"cardId\":25,\"color\":\"Green\",\"number\":\"Three\"},{\"cardId\":22,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":9,\"color\":\"White\",\"number\":\"Five\"},{\"cardId\":21,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":49,\"color\":\"Red\",\"number\":\"Five\"},{\"cardId\":20,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":37,\"color\":\"Blue\",\"number\":\"Four\"},{\"cardId\":36,\"color\":\"Blue\",\"number\":\"Three\"},{\"cardId\":15,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":31,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":0,\"color\":\"White\",\"number\":\"One\"},{\"cardId\":13,\"color\":\"Yellow\",\"number\":\"Two\"},{\"cardId\":46,\"color\":\"Red\",\"number\":\"Three\"},{\"cardId\":3,\"color\":\"White\",\"number\":\"Two\"},{\"cardId\":6,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":39,\"color\":\"Blue\",\"number\":\"Five\"},{\"cardId\":16,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":34,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":48,\"color\":\"Red\",\"number\":\"Four\"},{\"cardId\":23,\"color\":\"Green\",\"number\":\"Two\"},{\"cardId\":12,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":14,\"color\":\"Yellow\",\"number\":\"Two\"}],\"playedCards\":{\"White\":[{\"cardId\":2,\"color\":\"White\",\"number\":\"One\"}],\"Yellow\":[{\"cardId\":11,\"color\":\"Yellow\",\"number\":\"One\"}],\"Blue\":[{\"cardId\":33,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":32,\"color\":\"Blue\",\"number\":\"One\"}],\"Red\":[{\"cardId\":43,\"color\":\"Red\",\"number\":\"Two\"},{\"cardId\":41,\"color\":\"Red\",\"number\":\"One\"}]},\"discardedCards\":[{\"cardId\":30,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":1,\"color\":\"White\",\"number\":\"One\"}],\"hints\":5,\"fuckups\":1,\"lastPlayer\":null}"

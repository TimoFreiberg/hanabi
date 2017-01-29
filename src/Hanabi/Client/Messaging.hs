{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hanabi.Client.Messaging where

import qualified Data.Aeson as Aeson
import Data.Aeson
       (ToJSON, FromJSON, ToJSONKey, FromJSONKey, eitherDecode,
        genericToEncoding, genericToJSON, genericParseJSON, defaultOptions)
import Data.Aeson.Types
       (constructorTagModifier, sumEncoding, SumEncoding(..), camelTo2,
        sumEncoding)
import qualified Data.Aeson.Types as Aeson
import Data.Char (toUpper, toLower)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Prelude hiding (id)

import qualified Hanabi as Hanabi
import qualified Hanabi.Game as Hanabi

data Request
  = ConnectionRequest { name :: Text}
  | DiscardCardRequest { discarded_card :: Card}
  | HintColorRequest { target_player :: Text
                    ,  color :: Color}
  | HintNumberRequest { target_player :: Text
                     ,  number :: Number}
  | PlayCardRequest { played_card :: Card}
  | GameStartRequest
  deriving (Show, Generic)

instance FromJSON Request where
  parseJSON = genericParseJSON options

instance ToJSON Request where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

data Response
  = ErrorResponse { explanation :: Text
                 ,  err_details :: (Maybe Text)}
  | ConnectionResponse { names :: [Text]}
  | DiscardCardResponse { game_state :: GameState}
  | PlayCardResponse { game_state :: GameState}
  | HintColorResponse { game_state :: GameState}
  | HintNumberResponse { game_state :: GameState}
  | GameOverResponse { score :: Score}
  | GameStartResponse { game_state :: GameState}
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON = genericParseJSON options

instance ToJSON Response where
  toJSON = genericToJSON options
  toEncoding = genericToEncoding options

options :: Aeson.Options
options =
  Aeson.defaultOptions
  { constructorTagModifier = javaStyleTags
  , sumEncoding = TaggedObject "msg_type" "payload"
  }

javaStyleTags :: String -> String
javaStyleTags = map toUpper . camelTo2 '_'

newtype Score =
  Score Int
  deriving (Show, Generic, FromJSON, ToJSON)

data GameState = GameState
  { hint_tokens :: Int
  , hint_tokens_max :: Int
  , err_tokens :: Int
  , played_cards :: Map Color Number
  , players :: [Player]
  , deck :: Deck
  , discarded_cards :: [Card]
  , next_player :: Text
  , turns_left :: Maybe Int
  } deriving (Show, Generic, FromJSON, ToJSON)

data Deck = Deck
  { cards :: [Card]
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
  , knows_color_not :: Set Color
  , knows_number_not :: Set Number
  } deriving (Show, Generic, FromJSON, ToJSON)

data Card = Card
  { id :: Int
  , color :: Color
  , number :: Number
  } deriving (Show, Generic, FromJSON, ToJSON)

data Color
  = White
  | Yellow
  | Green
  | Blue
  | Red
  deriving (Show, Generic, Ord, Eq, Enum, Read)

instance ToJSONKey Color where
  toJSONKey = Aeson.toJSONKeyText (Text.pack . map toUpper . show)

instance FromJSONKey Color where
  fromJSONKey = Aeson.FromJSONKeyText (read . titleCase . Text.unpack)
    where
      titleCase [] = []
      titleCase (x:xs) = x : map toLower xs

instance FromJSON Color where
  parseJSON = Aeson.genericParseJSON capsOptions

instance ToJSON Color where
  toJSON = Aeson.genericToJSON capsOptions
  toEncoding = Aeson.genericToEncoding capsOptions

data Number
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Show, Generic, Ord, Eq, Enum, Bounded, Read)

instance ToJSONKey Number where
  toJSONKey = Aeson.toJSONKeyText (Text.pack . show)

instance FromJSONKey Number where
  fromJSONKey = Aeson.FromJSONKeyText (read . Text.unpack)

convert
  :: (Enum a, Enum b)
  => a -> b
convert = toEnum . fromEnum

instance FromJSON Number where
  parseJSON = Aeson.genericParseJSON capsOptions

instance ToJSON Number where
  toJSON = Aeson.genericToJSON capsOptions
  toEncoding = Aeson.genericToEncoding capsOptions

capsOptions :: Aeson.Options
capsOptions =
  defaultOptions
  {sumEncoding = UntaggedValue, constructorTagModifier = map toUpper}

toHanabi :: GameState -> Hanabi.Game
toHanabi (GameState hints _ errs played playerHands currentDeck discardedCards playerId turnsLeft) =
  Hanabi.Game
    (Hanabi.PlayerId playerId)
    mkHands
    mkDeck
    mkPlayed
    mkDiscarded
    hints
    errs
    turnsLeft
  where
    mkDeck = (\(Deck cs) -> map toCard cs) currentDeck
    mkDiscarded = map toCard discardedCards
    mkHands = foldr insertHand Map.empty playerHands
    insertHand (Player playerName cs) m =
      Map.insert (Hanabi.PlayerId playerName) (map toHand cs) m
    mkPlayed = foldr mkColorStack Map.empty (Map.assocs played)
    mkColorStack (col, num) m =
      Map.insert
        (convert col)
        (map
           (\n -> Hanabi.Card (-1) (convert col) n)
           [Hanabi.One .. (convert num)])
        m

fromHanabi :: Hanabi.Game -> GameState
fromHanabi (Hanabi.Game playerId hHands hDeck hPlayed hDiscarded hHints hErrs hTurnsLeft) =
  mkGameState
  where
    mkGameState =
      GameState
        hHints
        8
        hErrs
        mkPlayed
        mkHands
        mkDeck
        mkDiscarded
        (Hanabi.unPlayerId playerId)
        hTurnsLeft
    mkDeck = Deck (fmap fromCard hDeck)
    mkDiscarded = fmap fromCard hDiscarded
    mkPlayed =
      foldr
        (\(col, cs) m -> Map.insert (convert col) (getNum (myHead cs)) m)
        Map.empty
        (filter (not . null . snd) (Map.assocs hPlayed))
    getNum (Hanabi.Card _ _ n) = convert n
    mkHands = fmap fromHand (Map.assocs hHands)
    myHead (x:_) = x
    myHead [] = error "failed at converting played cards"

fromCard :: Hanabi.Card -> Card
fromCard (Hanabi.Card cardId col num) = Card cardId (convert col) (convert num)

toCard :: Card -> Hanabi.Card
toCard (Card cardId col num) = Hanabi.Card cardId (convert col) (convert num)

toHand :: CardInHand -> (Hanabi.Card, Set Hanabi.Fact)
toHand (CardInHand c kn) = (toCard c, mkFacts c kn)

fromHand :: (Hanabi.PlayerId, [(Hanabi.Card, Set Hanabi.Fact)]) -> Player
fromHand ((Hanabi.PlayerId playerId), hand) = Player playerId (fmap mkHand hand)
  where
    mkHand (hCard, facts) =
      CardInHand
        (fromCard hCard)
        (CardKnowledge posCol posNum (extractCols negCols) (extractNums negNums))
      where
        (numberFacts, colorFacts) = Set.partition Hanabi.isNumberFact facts
        (posCols, negCols) = Set.partition Hanabi.isPositiveFact colorFacts
        (posNums, negNums) = Set.partition Hanabi.isPositiveFact numberFacts
        posCol = not (Set.null posCols)
        posNum = not (Set.null posNums)
        extractCols = Set.map (\(Hanabi.Not (Hanabi.IsColor c)) -> convert c)
        extractNums = Set.map (\(Hanabi.Not (Hanabi.IsNumber n)) -> convert n)

mkFacts :: Card -> CardKnowledge -> Set Hanabi.Fact
mkFacts (Card _ col num) (CardKnowledge isCol isNum notCol notNum) =
  Set.unions [Set.fromList (colPos ++ numPos), colNeg, numNeg]
  where
    colPos =
      if isCol
        then [Hanabi.IsColor (convert col)]
        else []
    numPos =
      if isNum
        then [Hanabi.IsNumber (convert num)]
        else []
    colNeg = Set.map (Hanabi.Not . Hanabi.IsColor . convert) notCol
    numNeg = Set.map (Hanabi.Not . Hanabi.IsNumber . convert) notNum

fromRight
  :: Show l
  => Either l r -> r
fromRight (Right x) = x
fromRight (Left x) = error $ show x

exampleGame :: Hanabi.Game
exampleGame =
  fromRight . eitherDecode $
  "{\"activePlayer\":\"Alice\",\"playerHands\":{\"Alice\":[[{\"cardId\":47,\"color\":\"Red\",\"number\":\"Four\"},[]],[{\"cardId\":26,\"color\":\"Green\",\"number\":\"Three\"},[]],[{\"cardId\":19,\"color\":\"Yellow\",\"number\":\"Five\"},[]],[{\"cardId\":8,\"color\":\"White\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"cardId\":24,\"color\":\"Green\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]]],\"Bob\":[[{\"cardId\":4,\"color\":\"White\",\"number\":\"Two\"},[]],[{\"cardId\":45,\"color\":\"Red\",\"number\":\"Three\"},[]],[{\"cardId\":28,\"color\":\"Green\",\"number\":\"Four\"},[]],[{\"cardId\":7,\"color\":\"White\",\"number\":\"Four\"},[]],[{\"cardId\":27,\"color\":\"Green\",\"number\":\"Four\"},[]]],\"Charlie\":[[{\"cardId\":38,\"color\":\"Blue\",\"number\":\"Four\"},[]],[{\"cardId\":35,\"color\":\"Blue\",\"number\":\"Three\"},[]],[{\"cardId\":42,\"color\":\"Red\",\"number\":\"One\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"cardId\":44,\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"IsNumber\",\"contents\":\"Two\"},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}}]],[{\"cardId\":17,\"color\":\"Yellow\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]]]},\"deck\":[{\"cardId\":10,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":40,\"color\":\"Red\",\"number\":\"One\"},{\"cardId\":5,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":29,\"color\":\"Green\",\"number\":\"Five\"},{\"cardId\":18,\"color\":\"Yellow\",\"number\":\"Four\"},{\"cardId\":25,\"color\":\"Green\",\"number\":\"Three\"},{\"cardId\":22,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":9,\"color\":\"White\",\"number\":\"Five\"},{\"cardId\":21,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":49,\"color\":\"Red\",\"number\":\"Five\"},{\"cardId\":20,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":37,\"color\":\"Blue\",\"number\":\"Four\"},{\"cardId\":36,\"color\":\"Blue\",\"number\":\"Three\"},{\"cardId\":15,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":31,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":0,\"color\":\"White\",\"number\":\"One\"},{\"cardId\":13,\"color\":\"Yellow\",\"number\":\"Two\"},{\"cardId\":46,\"color\":\"Red\",\"number\":\"Three\"},{\"cardId\":3,\"color\":\"White\",\"number\":\"Two\"},{\"cardId\":6,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":39,\"color\":\"Blue\",\"number\":\"Five\"},{\"cardId\":16,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":34,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":48,\"color\":\"Red\",\"number\":\"Four\"},{\"cardId\":23,\"color\":\"Green\",\"number\":\"Two\"},{\"cardId\":12,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":14,\"color\":\"Yellow\",\"number\":\"Two\"}],\"playedCards\":{\"White\":[{\"cardId\":2,\"color\":\"White\",\"number\":\"One\"}],\"Yellow\":[{\"cardId\":11,\"color\":\"Yellow\",\"number\":\"One\"}],\"Blue\":[{\"cardId\":33,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":32,\"color\":\"Blue\",\"number\":\"One\"}],\"Red\":[{\"cardId\":43,\"color\":\"Red\",\"number\":\"Two\"},{\"cardId\":41,\"color\":\"Red\",\"number\":\"One\"}]},\"discardedCards\":[{\"cardId\":30,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":1,\"color\":\"White\",\"number\":\"One\"}],\"hints\":5,\"fuckups\":1,\"lastPlayer\":null}"

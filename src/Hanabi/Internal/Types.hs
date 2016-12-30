{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hanabi.Internal.Types where

import Data.String (IsString)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.List (isPrefixOf)

data Card = Card
  { _color :: Color
  , _number :: Number
  } deriving (Show, Eq, Ord, Generic)

data Color
  = White
  | Yellow
  | Green
  | Blue
  | Red
  deriving (Ord, Show, Eq, Bounded, Enum, Generic, Read)

data Number
  = One
  | Two
  | Three
  | Four
  | Five
  deriving (Ord, Show, Eq, Bounded, Enum, Generic)

newtype GameOver =
  GameOver Int
  deriving (Ord, Show, Eq, Generic)

makeLenses ''Card

newtype PlayerId =
  PlayerId Text
  deriving (Eq, Ord, Show, IsString, Generic, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

type Hand = [(Card, Set Fact)]

data Fact
  = IsColor Color
  | IsNumber Number
  | Not Fact
  deriving (Show, Eq, Ord, Generic)

data Game = Game
  { _actingPlayer :: PlayerId
  , _playerHands :: Map PlayerId Hand
  , _deck :: [Card]
  , _playedCards :: Map Color [Card]
  , _discardedCards :: [Card]
  , _hints :: Int
  , _fuckups :: Int
  , _lastPlayer :: Maybe PlayerId
  } deriving (Show, Eq, Generic)

data Hint
  = ColorHint Color
  | NumberHint Number
  deriving (Show, Generic)

class IsHint a  where
  toHint :: a -> Hint

instance IsHint Hint where
  toHint = id

instance IsHint Color where
  toHint = ColorHint

instance IsHint Number where
  toHint = NumberHint

makeLenses ''Game

instance ToJSON Number

instance ToJSON Color

instance ToJSON Hint

instance ToJSON Card where
  toEncoding = Aeson.genericToEncoding dropUnderscoreOptions

instance ToJSON Fact

instance ToJSONKey Color where
  toJSONKey = Aeson.toJSONKeyText (Text.pack . show)

instance ToJSON Game where
  toEncoding = Aeson.genericToEncoding dropUnderscoreOptions

instance FromJSON Number

instance FromJSON Color

instance FromJSON Hint

instance FromJSON Card where
  parseJSON = Aeson.genericParseJSON dropUnderscoreOptions

instance FromJSON Fact

instance FromJSONKey Color where
  fromJSONKey = Aeson.FromJSONKeyText (read . Text.unpack)

instance FromJSON Game where
  parseJSON = Aeson.genericParseJSON dropUnderscoreOptions

dropUnderscoreOptions :: Aeson.Options
dropUnderscoreOptions =
  Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropPrefixUnderscore
  }

dropPrefixUnderscore :: String -> String
dropPrefixUnderscore s
  | "_" `isPrefixOf` s = drop 1 s
  | otherwise = s

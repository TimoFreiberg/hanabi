{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hanabi.Types where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.String (IsString)
import Data.String.Conversions (convertString, ConvertibleStrings)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

data Card = Card
  { _cardId :: Int
  , _color :: Color
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
  deriving ( Eq
           , Ord
           , Show
           , IsString
           , Generic
           , FromJSON
           , ToJSON
           , ToJSONKey
           , FromJSONKey
           )

instance ConvertibleStrings PlayerId String where
  convertString = show

instance ConvertibleStrings String PlayerId where
  convertString = PlayerId . convertString

instance ConvertibleStrings PlayerId Text where
  convertString = unPlayerId

instance ConvertibleStrings Text PlayerId where
  convertString = PlayerId

unPlayerId :: PlayerId -> Text
unPlayerId (PlayerId x) = x

type Hand = [(Card, Set Fact)]

data Fact
  = IsColor Color
  | IsNumber Number
  | Not Fact
  deriving (Show, Eq, Ord, Generic)

data Game = Game
  { _activePlayer :: PlayerId
  , _playerHands :: Map PlayerId Hand
  , _deck :: [Card]
  , _playedCards :: Map Color [Card]
  , _discardedCards :: [Card]
  , _hints :: Int
  , _fuckups :: Int
  , _turnsLeft :: Maybe Int
  } deriving (Show, Eq, Generic)

data Hint
  = ColorHint Color
  | NumberHint Number
  deriving (Show, Generic)

class IsHint a where
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
  toJSON = Aeson.genericToJSON dropUnderscoreOptions
  toEncoding = Aeson.genericToEncoding dropUnderscoreOptions

instance ToJSON Fact

instance ToJSONKey Color where
  toJSONKey = Aeson.toJSONKeyText (Text.pack . show)

instance ToJSON Game where
  toJSON = Aeson.genericToJSON dropUnderscoreOptions
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
  Aeson.defaultOptions {Aeson.fieldLabelModifier = dropPrefixUnderscore}

dropPrefixUnderscore :: String -> String
dropPrefixUnderscore s
  | "_" `isPrefixOf` s = drop 1 s
  | otherwise = s

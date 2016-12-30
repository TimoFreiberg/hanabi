{-# LANGUAGE DeriveGeneric #-}

module Hanabi.Messages where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

import Hanabi

data Event
  = CardPlayed PlayerId
               Card
  | CardDiscarded PlayerId
                  Card
  | HintGained
  | HintSpent
  | FuckedUp
  | GameOver Int
  | HintGiven PlayerId
              Hint
  | CardDrawn PlayerId
              Card
  | PlayerTurn PlayerId
  deriving (Generic, Show)

data Action
  = GiveHint PlayerId
             Hint
  | PlayerCard Card
  | DiscardCard Card
  deriving (Generic, Show)

instance ToJSON Event

instance ToJSON Action

instance FromJSON Event

instance FromJSON Action

{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Print where

import Hanabi

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (convertString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

prettyPrint
  :: (MonadIO m, Pprint a)
  => a -> m ()
prettyPrint = liftIO . IO.putStrLn . pprint

selectiveFairPrint
  :: MonadIO m
  => PlayerId -> Game -> m ()
selectiveFairPrint player (Game actingPlayer' playerHands' deck' playedCards' discardedCards' hints' fuckups' turnsLeft') =
  liftIO $
  IO.putStrLn
    (Text.intercalate
       "\n"
       [ "Active: " <> pprint actingPlayer'
       , ""
       , "Hands:"
       , Text.concat
           [ pprint pId <> ":\n" <>
           Text.concat
             [ showT i <> ". " <>
             (if pId == player
                then ""
                else pprint card) <>
             " " <>
             pprint facts <>
             "\n"
             | (i, (card, facts)) <- zip [0 :: Int ..] hand
             ] <>
           "\n"
           | (pId, hand) <- Map.assocs playerHands'
           ]
       , "Played Cards:"
       , pprint playedCards'
       , "Discarded Cards:"
       , "  " <> pprint discardedCards'
       , "Cards in deck:"
       , "  " <> showT (length deck')
       , "hints: " <> showT hints'
       , "fuckups: " <> showT fuckups'
       , maybe "" (("\nTurns left: " <>) . showT) turnsLeft'
       ])

fairPrint
  :: MonadIO m
  => Game -> m ()
fairPrint game = selectiveFairPrint (view activePlayer game) game

class Pprint a where
  pprint :: a -> Text

instance Pprint Color where
  pprint = convertString . take 1 . show

instance Pprint Number where
  pprint = convertString . show . (+ 1) . fromEnum

instance Pprint Fact where
  pprint (Not f) = "!" <> pprint f
  pprint (IsColor c) = pprint c
  pprint (IsNumber n) = pprint n

instance Pprint Card where
  pprint card = "(" <> pprint col <> " " <> pprint num <> ")"
    where
      col = view color card
      num = view number card

instance Pprint Text where
  pprint = id

instance Pprint PlayerId where
  pprint (PlayerId s) = s

instance Pprint a =>
         Pprint [a] where
  pprint xs = Text.concat ["[", Text.intercalate ", " (map pprint xs), "]"]

instance Pprint a =>
         Pprint (NonEmpty a) where
  pprint = pprint . toList

instance Pprint a =>
         Pprint (Set a) where
  pprint = pprint . Set.elems

instance (Pprint k, Pprint v) =>
         Pprint (Map k v) where
  pprint m =
    Text.concat [pprint k <> ":" <> pprint v <> "\n" | (k, v) <- Map.assocs m]

instance Pprint Game where
  pprint (Game actingPlayer' playerHands' deck' playedCards' discardedCards' hints' fuckups' lastPlayer') =
    Text.intercalate
      "\n"
      [ "Active: " <> pprint actingPlayer'
      , ""
      , "Hands:"
      , Text.concat
          [ pprint pId <> ":\n" <>
          Text.concat
            [ showT i <> ". " <> pprint card <> " " <> pprint facts <> "\n"
            | (i, (card, facts)) <- zip [0 :: Int ..] hand
            ] <>
          "\n"
          | (pId, hand) <- Map.assocs playerHands'
          ]
      , "Played Cards:"
      , pprint playedCards'
      , "Discarded Cards:"
      , "  " <> pprint discardedCards'
      , "hints: " <> showT hints'
      , "fuckups: " <> showT fuckups'
      , case lastPlayer' of
          Nothing -> ""
          Just lastPlayerId -> "\nLast Player: " <> showT lastPlayerId
      , "Deck:"
      , pprint deck'
      ]

showT
  :: Show a
  => a -> Text
showT = convertString . show

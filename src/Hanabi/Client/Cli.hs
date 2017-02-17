{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Hanabi.Client.Cli where

import Control.Concurrent.MVar (tryReadMVar)
import Control.Lens (view, at, non, to)
import Control.Monad ((>=>), guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List.NonEmpty as List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import Data.String.Conversions (convertString, ConvertibleStrings)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Read (readMaybe)

import qualified Hanabi
import Hanabi.Client.Messaging

data InputReaction
  = SendRequest Request
  | InputErr Text

inputHandler gameStore playerName input =
  case input of
    "start" -> SendRequest GameStartRequest
    (isCommand input -> Just command) -> do
      games <- tryReadMVar gameStore
      return $
        case games of
          Nothing -> InputErr "Game has not started yet"
          Just (game :| _) ->
            case input of
              (checkPlay game playerName -> Just playWhat) -> do
                (SendRequest
                   (PlayCardRequest (getCardIdAt game playerName playWhat)))
              (checkDiscard game playerName -> Just discardWhat) -> do
                (SendRequest
                   (DiscardCardRequest (getCardIdAt game playerName discardWhat)))
              (checkHint game playerName >=> extractColorHint -> Just (hintWhom, hintColor)) -> do
                SendRequest (HintColorRequest hintWhom hintColor)
              (checkHint game playerName >=> extractNumberHint -> Just (hintWhom, hintNumber)) -> do
                SendRequest (HintNumberRequest hintWhom hintNumber)
              _ -> InputErr "couldn't read input"

isCommand s
  | any (`Text.isPrefixOf` s) ["play", "discard", "hint"] = Just s
  | otherwise = Nothing

extractNumberHint :: (a, Text) -> Maybe (a, Number)
extractNumberHint (x, s) =
  case Text.toLower s of
    "1" -> Just (x, One)
    "2" -> Just (x, Two)
    "3" -> Just (x, Three)
    "4" -> Just (x, Four)
    "5" -> Just (x, Five)
    "one" -> Just (x, One)
    "two" -> Just (x, Two)
    "three" -> Just (x, Three)
    "four" -> Just (x, Four)
    "five" -> Just (x, Five)
    _ -> Nothing

extractColorHint :: (a, Text) -> Maybe (a, Color)
extractColorHint (x, c) = fmap (x, ) (toColorHint c)

toColorHint :: Text -> Maybe Color
toColorHint s
  | ((Text.toLower s) `Text.isPrefixOf` "white") = Just White
  | ((Text.toLower s) `Text.isPrefixOf` "yellow") = Just Yellow
  | ((Text.toLower s) `Text.isPrefixOf` "green") = Just Green
  | ((Text.toLower s) `Text.isPrefixOf` "blue") = Just Blue
  | ((Text.toLower s) `Text.isPrefixOf` "red") = Just Red
  | otherwise = Nothing

checkDiscard :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe Int
checkDiscard game name input = do
  discardWhat <- Text.stripPrefix "discard" input
  i <- readT discardWhat
  checkCardIndex game name i
  --FIXME

checkPlay :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe Int
checkPlay game name input = do
  playWhat <- Text.stripPrefix "play" input
  i <- readT playWhat
  checkCardIndex game name i
  --FIXME

checkHint :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe (Text, Text)
checkHint game name input = do
  params <- Text.stripPrefix "hint" input
  let tokens = Text.words params
  guard (length tokens >= 2)
  let hintWhom = Text.concat (init tokens)
  let hintWhat = last tokens
  guard (hintWhom /= (convertString name))
  guard
    ((Hanabi.PlayerId hintWhom) `elem`
     (view (Hanabi.playerHands . to Map.keys) game))
  return (hintWhom, hintWhat)

checkCardIndex :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Maybe Int
checkCardIndex game name i
  | i >= 0 && i < handSize = Just i
  | otherwise = Nothing
  where
    handSize = length (myHand game name)

getCardIdAt :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Int
getCardIdAt g n i = view (Hanabi.cardId) (fst ((myHand g n) !! i))

myHand :: Hanabi.Game -> Hanabi.PlayerId -> Hanabi.Hand
myHand game name = view (Hanabi.playerHands . at name . non []) game

ask
  :: MonadIO m
  => Text -> m Text
ask msg =
  liftIO $ do
    IO.putStrLn msg
    IO.getLine

getLn
  :: MonadIO m
  => m Text
getLn = liftIO IO.getLine

putLn
  :: (MonadIO m)
  => Text -> m ()
putLn = liftIO . IO.putStrLn . convertString

readT
  :: (ConvertibleStrings a String, Read b)
  => a -> Maybe b
readT = readMaybe . convertString

showT
  :: (Show a, ConvertibleStrings String b)
  => a -> b
showT = convertString . show

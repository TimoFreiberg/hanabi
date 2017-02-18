{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Hanabi.Client.Cli
  ( inputHandler
  , ask
  , getLn
  , putLn
  , showT
  , InputReaction(..)
  ) where

import Control.Concurrent.MVar (tryReadMVar, MVar)
import Control.Lens (view, at, non, to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.String.Conversions (convertString, ConvertibleStrings)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Read (readMaybe)

import qualified Hanabi
import qualified Hanabi.Client.Messaging as Msg

data InputReaction
  = SendRequest Msg.Request
  | InputErr Text
  deriving (Show)

type Parsed a = Either Text a

dispatchInput :: Parsed Msg.Request -> InputReaction
dispatchInput = either InputErr SendRequest

inputHandler
  :: MonadIO m
  => (MVar (NonEmpty Hanabi.Game)) -> Hanabi.PlayerId -> Text -> m InputReaction
inputHandler gameStore playerName input =
  case input of
    "start" -> return (SendRequest Msg.GameStartRequest)
    (isCommand -> Just _) -> do
      games <- liftIO (tryReadMVar gameStore)
      pure
        (case games of
           Nothing -> InputErr "Game has not started yet"
           Just (game :| _) ->
             case input of
               (checkPlay game playerName -> Just playWhat) -> do
                 dispatchInput (fmap Msg.PlayCardRequest playWhat)
               (checkDiscard game playerName -> Just discard) -> do
                 dispatchInput (fmap Msg.DiscardCardRequest discard)
               (checkHint game playerName -> Just hint) ->
                 case hint of
                   (Left err) -> InputErr err
                   (parseColorHint -> Right (hintWhom, col)) ->
                     SendRequest (Msg.HintColorRequest hintWhom col)
                   (parseNumberHint -> Right (hintWhom, num)) ->
                     SendRequest (Msg.HintNumberRequest hintWhom num)
                   _ ->
                     InputErr
                       "Hints must end with a number (e.g. 1 or one) or a color (e.g. blue or yellow)"
               _ -> InputErr "couldn't read input")
    _ ->
      pure
        (InputErr
           ("Available commands are:" <>
            Text.unlines
              [ "'start'"
              , "'play' " <> cardIndexMsg
              , "discard " <> cardIndexMsg
              , "hint [player name] [color or number]"
              ]))
  where
    cardIndexMsg = "[card index, e.g. 0]"

isCommand :: Text -> Maybe Text
isCommand s
  | any (`Text.isPrefixOf` s) ["play", "discard", "hint"] = Just s
  | otherwise = Nothing

parseNumberHint :: Parsed (a, Text) -> Parsed (a, Msg.Number)
parseNumberHint = (>>= traverse parseNumber)

parseNumber :: Text -> Either Text Msg.Number
parseNumber s =
  case Text.toLower s of
    "1" -> Right Msg.One
    "one" -> Right Msg.One
    "2" -> Right Msg.Two
    "two" -> Right Msg.Two
    "3" -> Right Msg.Three
    "three" -> Right Msg.Three
    "4" -> Right Msg.Four
    "four" -> Right Msg.Four
    "5" -> Right Msg.Five
    "five" -> Right Msg.Five
    _ -> Left ("Failed to interpret " <> s <> " as a number hint.")

parseColorHint :: Parsed (a, Text) -> Parsed (a, Msg.Color)
parseColorHint = (>>= traverse parseColor)

parseColor :: Text -> Either Text Msg.Color
parseColor s
  | startsWith "white" = Right Msg.White
  | startsWith "yellow" = Right Msg.Yellow
  | startsWith "green" = Right Msg.Green
  | startsWith "blue" = Right Msg.Blue
  | startsWith "red" = Right Msg.Red
  | otherwise = Left ("Failed to interpret " <> s <> " as a color hint.")
  where
    startsWith = Text.isPrefixOf (Text.toLower s)

checkDiscard :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe (Parsed Int)
checkDiscard game name input = do
  discardWhat <- Text.stripPrefix "discard" input
  i <- readT discardWhat
  pure (getCardIdAt game name i)

checkPlay :: Hanabi.Game -> Hanabi.PlayerId -> Text -> Maybe (Parsed Int)
checkPlay game name input = do
  playWhat <- Text.stripPrefix "play" input
  i <- readT playWhat
  pure (getCardIdAt game name i)

checkHint :: Hanabi.Game
          -> Hanabi.PlayerId
          -> Text
          -> Maybe (Parsed (Text, Text))
checkHint game name input = do
  params <- Text.stripPrefix "hint" input
  pure
    (do let tokens = Text.words params
        guardMsg
          "'hint' must be followed by the name of the targeted player and a number or color."
          (length tokens >= 2)
        let hintWhom = Text.concat (init tokens)
        guardMsg "You can't hint yourself." (hintWhom /= (convertString name))
        let hintWhat = last tokens
        guardMsg
          ("Player " <> hintWhom <> " not found.")
          ((Hanabi.PlayerId hintWhom) `elem`
           (view (Hanabi.playerHands . to Map.keys) game))
        pure (hintWhom, hintWhat))

guardMsg :: Text -> Bool -> Parsed ()
guardMsg _ True = Right ()
guardMsg msg False = Left msg

getCardIdAt :: Hanabi.Game -> Hanabi.PlayerId -> Int -> Parsed Int
getCardIdAt g n i
  | i >= 0 && i < length hand = Right (hand !! i)
  | otherwise = Left ("No card at position" <> showT i)
  where
    hand = fmap (view Hanabi.cardId . fst) (myHand g n)

myHand :: Hanabi.Game -> Hanabi.PlayerId -> Hanabi.Hand
myHand g n = view (Hanabi.playerHands . at n . non []) g

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

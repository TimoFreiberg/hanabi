{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Client.Cli where

import Data.Text (Text)
import qualified Data.Text as Text
import Hanabi.Client.Messaging

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

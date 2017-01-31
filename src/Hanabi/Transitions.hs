{-# LANGUAGE ViewPatterns #-}

module Hanabi.Transitions where

import Hanabi
import Hanabi.Game

cardPlayed :: Card -> Game -> Either GameOver Game
cardPlayed card = endTurn . drawCard . tryPlay . removeFromHand card
  where
    tryPlay game =
      if canPlayCard card game
        then (checkHintBonus . putOnPlayedStack card) game
        else (recordFailure . putOnDiscardedStack card) game
    checkHintBonus
      | isFive card = incrementHintCount
      | otherwise = id

cardDiscarded :: Card -> Game -> Either GameOver Game
cardDiscarded card =
  endTurn .
  drawCard . incrementHintCount . putOnDiscardedStack card . removeFromHand card

hintGiven
  :: IsHint hint
  => hint -> PlayerId -> Game -> Either GameOver Game
hintGiven hint playerId =
  endTurn . decrementHintCount . giveHint (toHint hint) playerId

endTurn :: Game -> Either GameOver Game
endTurn (checkLastRound -> game)
  | any ($ game) [lastRoundFinished, tooManyFailures, allStacksFilled] =
    Left (gameOver game)
  | otherwise = (Right . nextPlayer) game

checkLastRound :: Game -> Game
checkLastRound game
  | deckEmpty game = startLastRound game
  | otherwise = game

gameOver :: Game -> GameOver
gameOver game = GameOver (getScore game)

drawCard :: Game -> Game
drawCard game =
  case popFromDeck game of
    Just (card, game') -> addToHand card game'
    Nothing -> game

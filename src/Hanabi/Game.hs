{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hanabi.Game where

import Control.Lens
       (over, view, to, ix, traversed, at, non, set, lens, Lens', mapped)

import Control.Arrow ((>>>))
import Data.Function (on)
import qualified Data.List as List (delete, deleteBy)
import qualified Data.List.NonEmpty as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import System.Random.Shuffle (shuffleM)

import Hanabi

colors :: [Color]
colors = [minBound .. maxBound]

numbers :: [Number]
numbers = [minBound .. maxBound]

occurrences :: Number -> Int
occurrences One = 3
occurrences Two = 2
occurrences Three = 2
occurrences Four = 2
occurrences Five = 1

handSize :: Int -> Int
handSize 2 = 5
handSize 3 = 5
handSize 4 = 4
handSize 5 = 4
handSize n = error ("the game does not support " ++ show n ++ " players!")

maximumFailures :: Int
maximumFailures = 3

initialHints :: Int
initialHints = 8

sortedGame :: [Card]
sortedGame = zipWith (set cardId) [0 ..] (concat (concat allCards))
  where
    numberCards col num = replicate (occurrences num) (Card (-1) col num)
    colorCards col = map (numberCards col) numbers
    allCards = map colorCards colors

dealCards :: Int -> IO ([[Card]], [Card])
dealCards playerCount = do
  cards <- shuffleM sortedGame
  let (hands', dk) = splitAt (playerCount * handSize playerCount) cards
  let hands = inGroupsOf (handSize playerCount) hands'
  return (hands, dk)

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf _ [] = []
inGroupsOf n xs =
  let (as, bs) = splitAt n xs
  in as : inGroupsOf n bs

isSuccessor
  :: (Bounded t, Enum t, Eq t)
  => t -> t -> Bool
num1 `isSuccessor` num2 = (num1, num2) `elem` zip allNums (tail allNums)
  where
    allNums = [minBound .. maxBound]

getCards :: Hand -> [Card]
getCards = map fst

createHand :: [Card] -> Hand
createHand = fmap (, Set.empty)

isPositiveFact :: Fact -> Bool
isPositiveFact (Not _) = False
isPositiveFact _ = True

isNumberFact :: Fact -> Bool
isNumberFact (Not (IsNumber _)) = True
isNumberFact (IsNumber _) = True
isNumberFact _ = False

differentType :: Fact -> Fact -> Bool
differentType = (/=) `on` isNumberFact

isColorFact :: Fact -> Bool
isColorFact = not . isNumberFact

-- propNNumbers :: Int -> Number -> Color -> Bool
-- propNNumbers n num col = length ones == n
--   where
--     ones =
--         filter
--             (\(Card c n) ->
--                   c == col && n == num)
--             sortedGame
-- spec :: IO ()
-- spec =
--     hspec $
--     describe "the sorted game" $
--     do it "has three ones" $ property $ propNNumbers 3 One
--        it "has two twos" $ property $ propNNumbers 2 Two
--        it "has two threes" $ property $ propNNumbers 2 Three
--        it "has two fours" $ property $ propNNumbers 2 Four
--        it "has one five" $ property $ propNNumbers 1 Five
initState :: PlayerId -> [PlayerId] -> IO Game
initState startId ids = do
  (cards, dk) <- dealCards (length cleanIds)
  let hands = fmap createHand cards
  let players = Map.fromList (zip cleanIds hands)
  return (Game startId players dk Map.empty [] initialHints 0 Nothing)
  where
    cleanIds = startId : List.delete startId ids

canPlayCard :: Card -> Game -> Bool
canPlayCard card game =
  case getStack (view color card) game of
    [] -> (num card) == One
    card2:_ -> (num card2) `isSuccessor` (num card)
  where
    num = view number

isFive :: Card -> Bool
isFive card = view number card == Five

tooManyFailures :: Game -> Bool
tooManyFailures game = view fuckups game >= maximumFailures

allStacksFilled :: Game -> Bool
allStacksFilled game =
  length stacks == length colors &&
  all (\stack -> length stack == length numbers) stacks
  where
    stacks = view (playedCards . to Map.elems) game

getStack :: Color -> Game -> [Card]
getStack col = view (playedCards . ix col . to List.toList)

removeFromHand :: Card -> Game -> Game
removeFromHand card =
  over activeHand (List.deleteBy ((==) `on` fst) (card, Set.empty))

putOnPlayedStack :: Card -> Game -> Game
putOnPlayedStack card =
  over
    (playedCards . at (view color card))
    (Just . maybe (List.fromList [card]) (card List.<|))

putOnDiscardedStack :: Card -> Game -> Game
putOnDiscardedStack card = over discardedCards (card :)

recordFailure :: Game -> Game
recordFailure = over fuckups (+ 1)

incrementHintCount :: Game -> Game
incrementHintCount = over hints (+ 1)

decrementHintCount :: Game -> Game
decrementHintCount = over hints (subtract 1)

giveHint :: Hint -> PlayerId -> Game -> Game
giveHint hint player = over (playerHands . at player . non []) (applyHint hint)

applyHint :: Hint -> Hand -> Hand
applyHint (ColorHint hintCol) =
  over
    traversed
    (\(card, facts) -> (card, insertFact (makeFact (view color card)) facts))
  where
    makeFact cardCol
      | hintCol == cardCol = IsColor hintCol
      | otherwise = Not (IsColor hintCol)
applyHint (NumberHint hintNum) =
  over
    traversed
    (\(card, facts) -> (card, insertFact (makeFact (view number card)) facts))
  where
    makeFact cardNum
      | hintNum == cardNum = IsNumber hintNum
      | otherwise = Not (IsNumber hintNum)

insertFact :: Fact -> Set Fact -> Set Fact
insertFact fact@(Not _) facts
  | containsPositive = facts
  | otherwise = Set.insert fact facts
  where
    containsPositive =
      any (\f -> isPositiveFact f && not (fact `differentType` f)) facts
insertFact fact facts =
  Set.insert
    fact
    (Set.filter (\f -> isPositiveFact f || fact `differentType` f) facts)

nextPlayer :: Game -> Game
nextPlayer game = set activePlayer playerAfter game
  where
    oldPlayer = view activePlayer game
    playerAfter =
      view
        (playerHands .
         to (Map.keys >>> cycle >>> dropWhile (/= oldPlayer) >>> (!! 1)))
        game

popFromDeck :: Game -> Maybe (Card, Game)
popFromDeck game =
  case view deck game of
    [] -> Nothing
    (card:_) -> Just (card, over deck (drop 1) game)

addToHand :: Card -> Game -> Game
addToHand card = over activeHand ((card, Set.empty) :)

startLastRound :: Game -> Game
startLastRound game =
  case view turnsLeft game of
    Nothing -> set turnsLeft (Just (view (playerHands . to Map.size) game)) game
    Just _ -> over (turnsLeft . mapped) (subtract 1) game

deckEmpty :: Game -> Bool
deckEmpty = view (deck . to null)

lastRoundFinished :: Game -> Bool
lastRoundFinished game = maybe False (0 ==) (view turnsLeft game)

getScore :: Game -> Int
getScore =
  sum . view (playedCards . to (fmap (numToInt . view number . List.head)))

numToInt :: Number -> Int
numToInt One = 1
numToInt Two = 2
numToInt Three = 3
numToInt Four = 4
numToInt Five = 5

exampleGame :: IO Game
exampleGame = initState "1" ["2", "3"]

cardAt :: Int -> Game -> Card
cardAt i = view (activeHand . to (fmap fst >>> (!! i)))

activeHand :: Lens' Game Hand
activeHand =
  lens
    (\game -> view (playerHands . at (activeP game) . non []) game)
    (\game hand -> set (playerHands . at (activeP game) . non []) hand game)

activeP :: Game -> PlayerId
activeP = view activePlayer

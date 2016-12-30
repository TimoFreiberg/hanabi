module Hanabi.Print where

import Hanabi

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (intercalate)

prettyPrint
  :: Pprint a
  => a -> IO ()
prettyPrint = putStrLn . pprint

fairPrint :: Game -> IO ()
fairPrint (Game actingPlayer' playerHands' _ playedCards' discardedCards' hints' fuckups' lastPlayer') =
  putStrLn
    (intercalate
       "\n"
       [ "Active: " ++ pprint actingPlayer'
       , ""
       , "Hands:"
       , concat
           [ pprint pId ++
            ":\n" ++
            concat
              [ show i ++
               ". " ++
               (if pId == actingPlayer'
                  then ""
                  else pprint card) ++
               " " ++ pprint facts ++ "\n"
              | (i, (card, facts)) <- zip [0 :: Int ..] hand ] ++
            "\n"
           | (pId, hand) <- Map.assocs playerHands' ]
       , "Played Cards:"
       , pprint playedCards'
       , "Discarded Cards:"
       , "  " ++ pprint discardedCards'
       , "hints: " ++ show hints'
       , "fuckups: " ++ show fuckups'
       , case lastPlayer' of
           Nothing -> ""
           Just lastPlayerId -> "\nLast Player: " ++ show lastPlayerId
       ])

class Pprint a  where
  pprint :: a -> String

instance Pprint Color where
  pprint = take 1 . show

instance Pprint Number where
  pprint = show . (+ 1) . fromEnum

instance Pprint Fact where
  pprint (Not f) = "!" ++ pprint f
  pprint (IsColor c) = pprint c
  pprint (IsNumber n) = pprint n

instance Pprint Card where
  pprint (Card col num) = "(" ++ pprint col ++ " " ++ pprint num ++ ")"

instance Pprint Text where
  pprint = Text.unpack

instance Pprint PlayerId where
  pprint (PlayerId s) = "Player " ++ pprint s

instance Pprint a =>
         Pprint [a] where
  pprint xs = concat ["[", intercalate ", " (map pprint xs), "]"]

instance Pprint a =>
         Pprint (Set a) where
  pprint = pprint . Set.elems

instance (Pprint k, Pprint v) =>
         Pprint (Map k v) where
  pprint m =
    concat
      [ pprint k ++ ":" ++ pprint v ++ "\n"
      | (k, v) <- Map.assocs m ]

instance Pprint Game where
  pprint (Game actingPlayer' playerHands' deck' playedCards' discardedCards' hints' fuckups' lastPlayer') =
    intercalate
      "\n"
      [ "Active: " ++ pprint actingPlayer'
      , ""
      , "Hands:"
      , concat
          [ pprint pId ++
           ":\n" ++
           concat
             [ show i ++ ". " ++ pprint card ++ " " ++ pprint facts ++ "\n"
             | (i, (card, facts)) <- zip [0 :: Int ..] hand ] ++
           "\n"
          | (pId, hand) <- Map.assocs playerHands' ]
      , "Played Cards:"
      , pprint playedCards'
      , "Discarded Cards:"
      , "  " ++ pprint discardedCards'
      , "hints: " ++ show hints'
      , "fuckups: " ++ show fuckups'
      , case lastPlayer' of
          Nothing -> ""
          Just lastPlayerId -> "\nLast Player: " ++ show lastPlayerId
      , "Deck:"
      , pprint deck'
      ]

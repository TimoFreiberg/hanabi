{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hanabi.Ui where

import qualified Brick as Brk
import qualified Brick.AttrMap as Brk
import qualified Brick.Main as Brk
import qualified Brick.Types as Brk
import qualified Brick.Widgets.Center as Brk
import Brick.Widgets.Core ((<+>), (<=>), hLimit, vLimit, txt)
import qualified Brick.Widgets.Edit as Brk
import Control.Lens ((^.), makeLenses, to, ix)
import Data.Aeson (eitherDecode)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.String.Conversions (convertString, ConvertibleStrings)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as Brk

import qualified Hanabi
import Hanabi (playerHands)
import qualified Hanabi.Game as Hanabi
import qualified Hanabi.Print as Print

data WName =
  TextField
  deriving (Show, Eq, Ord)

data State = State
  { _games :: NonEmpty Hanabi.Game
  , _playerName :: Hanabi.PlayerId
  , _mode :: Mode
  }

data Mode
  = Waiting
  | Choosing Action
  | Playing Hanabi.Card
  | Discarding Hanabi.Card
  | HintingWhom Hanabi.PlayerId
  | HintingWhat Hanabi.Hint

data Action
  = Play
  | Discard
  | Hint

makeLenses ''State

drawUI :: State -> [Brk.Widget WName]
drawUI st = [ui]
  where
    player = st ^. playerName
    currentGame = st ^. games . to NE.head
    ui =
      Brk.center
        (txt (Print.selectiveFairPrint player currentGame) <=>
         (Brk.padTop (Brk.Pad 5) (txt (showInput (st ^. mode)))))
    showInput Waiting = "Waiting for your turn..."
    showInput (Choosing _) =
      Text.unlines $
      fmap
        hotkeyize
        ["Play Card" :: String, "Discard Card", "Hint another Player"]
    showInput (Playing _) = "Play Card:\n" <> listCards
    showInput (Discarding _) = "Discard Card:\n" <> listCards
    showInput (HintingWhom _) = "Hint player:\n" <> listOtherPlayers
    showInput (HintingWhat _) =
      "Give color or number hint:\n" <> listColorHints <> listNumberHints
    listOtherPlayers =
      Text.unlines . numberList $
      (currentGame ^. playerHands . to Map.keys . to (filter (/= player)) .
       to (fmap Hanabi.unPlayerId))
    listCards =
      Text.unlines
        (fmap
           hotkeyize
           (numberList
              (Print.hiddenHand $ currentGame ^. playerHands . ix player)))
    listColorHints = Text.unlines (fmap (hotkeyize . show) Hanabi.colors)
    listNumberHints = Text.unlines (fmap (hotkeyize . show) Hanabi.numbers)
    numberList = zipWith (<>) ((<> ": ") . Text.pack . show <$> [1 :: Int ..])

hotkeyize
  :: ConvertibleStrings a String
  => a -> Text
hotkeyize = Text.pack . parenHead . convertString
  where
    parenHead [] = []
    parenHead (x:xs) = '(' : x : ')' : xs

appEvent :: State -> Brk.BrickEvent WName e -> Brk.EventM WName (Brk.Next State)
appEvent st (Brk.VtyEvent ev) =
  case ev of
    Brk.EvKey Brk.KEsc [] -> Brk.halt st
    Brk.EvKey (Brk.KChar '\t') [] -> Brk.continue st
    _ -> Brk.halt st --Brk.continue =<< undefined -- FIXME handle input
appEvent st _ = Brk.continue st

theMap :: Brk.AttrMap
theMap = Brk.attrMap Brk.defAttr []

initState = State (exampleGame :| []) "Alice" (Choosing Play)

app :: Brk.App State e WName
app =
  Brk.App
  { Brk.appDraw = drawUI
  , Brk.appChooseCursor = Brk.showFirstCursor
  , Brk.appHandleEvent = appEvent
  , Brk.appStartEvent = return
  , Brk.appAttrMap = const theMap
  }

startApp = Brk.defaultMain app initState

fromRight
  :: Show l
  => Either l r -> r
fromRight (Right x) = x
fromRight (Left x) = error $ show x

exampleGame :: Hanabi.Game
exampleGame =
  fromRight . eitherDecode $
  "{\"activePlayer\":\"Alice\",\"playerHands\":{\"Alice\":[[{\"cardId\":47,\"color\":\"Red\",\"number\":\"Four\"},[]],[{\"cardId\":26,\"color\":\"Green\",\"number\":\"Three\"},[]],[{\"cardId\":19,\"color\":\"Yellow\",\"number\":\"Five\"},[]],[{\"cardId\":8,\"color\":\"White\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"cardId\":24,\"color\":\"Green\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]]],\"Bob\":[[{\"cardId\":4,\"color\":\"White\",\"number\":\"Two\"},[]],[{\"cardId\":45,\"color\":\"Red\",\"number\":\"Three\"},[]],[{\"cardId\":28,\"color\":\"Green\",\"number\":\"Four\"},[]],[{\"cardId\":7,\"color\":\"White\",\"number\":\"Four\"},[]],[{\"cardId\":27,\"color\":\"Green\",\"number\":\"Four\"},[]]],\"Charlie\":[[{\"cardId\":38,\"color\":\"Blue\",\"number\":\"Four\"},[]],[{\"cardId\":35,\"color\":\"Blue\",\"number\":\"Three\"},[]],[{\"cardId\":42,\"color\":\"Red\",\"number\":\"One\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"cardId\":44,\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"IsNumber\",\"contents\":\"Two\"},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}}]],[{\"cardId\":17,\"color\":\"Yellow\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]]]},\"deck\":[{\"cardId\":10,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":40,\"color\":\"Red\",\"number\":\"One\"},{\"cardId\":5,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":29,\"color\":\"Green\",\"number\":\"Five\"},{\"cardId\":18,\"color\":\"Yellow\",\"number\":\"Four\"},{\"cardId\":25,\"color\":\"Green\",\"number\":\"Three\"},{\"cardId\":22,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":9,\"color\":\"White\",\"number\":\"Five\"},{\"cardId\":21,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":49,\"color\":\"Red\",\"number\":\"Five\"},{\"cardId\":20,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":37,\"color\":\"Blue\",\"number\":\"Four\"},{\"cardId\":36,\"color\":\"Blue\",\"number\":\"Three\"},{\"cardId\":15,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":31,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":0,\"color\":\"White\",\"number\":\"One\"},{\"cardId\":13,\"color\":\"Yellow\",\"number\":\"Two\"},{\"cardId\":46,\"color\":\"Red\",\"number\":\"Three\"},{\"cardId\":3,\"color\":\"White\",\"number\":\"Two\"},{\"cardId\":6,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":39,\"color\":\"Blue\",\"number\":\"Five\"},{\"cardId\":16,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":34,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":48,\"color\":\"Red\",\"number\":\"Four\"},{\"cardId\":23,\"color\":\"Green\",\"number\":\"Two\"},{\"cardId\":12,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":14,\"color\":\"Yellow\",\"number\":\"Two\"}],\"playedCards\":{\"White\":[{\"cardId\":2,\"color\":\"White\",\"number\":\"One\"}],\"Yellow\":[{\"cardId\":11,\"color\":\"Yellow\",\"number\":\"One\"}],\"Blue\":[{\"cardId\":33,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":32,\"color\":\"Blue\",\"number\":\"One\"}],\"Red\":[{\"cardId\":43,\"color\":\"Red\",\"number\":\"Two\"},{\"cardId\":41,\"color\":\"Red\",\"number\":\"One\"}]},\"discardedCards\":[{\"cardId\":30,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":1,\"color\":\"White\",\"number\":\"One\"}],\"hints\":5,\"fuckups\":1,\"lastPlayer\":null}"

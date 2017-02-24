{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Ui where

import qualified Brick as Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ((<+>), (<=>), hLimit, vLimit, txt)
import qualified Brick.Widgets.Edit as E
import Control.Lens ((^.), makeLenses)
import Data.Aeson (eitherDecode)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import qualified Hanabi

data WName =
  TextField
  deriving (Show, Eq, Ord)

data State = State
  { _games :: NonEmpty Hanabi.Game
  , _playerName :: Hanabi.PlayerId
  , _edit :: E.Editor Text WName
  }

makeLenses ''State

drawUI :: State -> [T.Widget WName]
drawUI st = [ui]
  where
    e = E.renderEditor True (st ^. edit)
    ui =
      C.center
        (txt "text field todo" <=> txt "enter command here" <+> (hLimit 50 e))

appEvent :: State -> T.BrickEvent WName e -> T.EventM WName (T.Next State)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue st
    _ -> M.continue =<< T.handleEventLensed st edit E.handleEditorEvent ev
appEvent st _ = M.continue st

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

initState =
  State
    (exampleGame :| [])
    "Alice"
    (E.editor TextField (txt . Text.unlines) (Just 1) "")

app :: M.App State e WName
app =
  M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const theMap
  }

startApp = M.defaultMain app initState

fromRight
  :: Show l
  => Either l r -> r
fromRight (Right x) = x
fromRight (Left x) = error $ show x

exampleGame :: Hanabi.Game
exampleGame =
  fromRight . eitherDecode $
  "{\"activePlayer\":\"Alice\",\"playerHands\":{\"Alice\":[[{\"cardId\":47,\"color\":\"Red\",\"number\":\"Four\"},[]],[{\"cardId\":26,\"color\":\"Green\",\"number\":\"Three\"},[]],[{\"cardId\":19,\"color\":\"Yellow\",\"number\":\"Five\"},[]],[{\"cardId\":8,\"color\":\"White\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]],[{\"cardId\":24,\"color\":\"Green\",\"number\":\"Two\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}}]]],\"Bob\":[[{\"cardId\":4,\"color\":\"White\",\"number\":\"Two\"},[]],[{\"cardId\":45,\"color\":\"Red\",\"number\":\"Three\"},[]],[{\"cardId\":28,\"color\":\"Green\",\"number\":\"Four\"},[]],[{\"cardId\":7,\"color\":\"White\",\"number\":\"Four\"},[]],[{\"cardId\":27,\"color\":\"Green\",\"number\":\"Four\"},[]]],\"Charlie\":[[{\"cardId\":38,\"color\":\"Blue\",\"number\":\"Four\"},[]],[{\"cardId\":35,\"color\":\"Blue\",\"number\":\"Three\"},[]],[{\"cardId\":42,\"color\":\"Red\",\"number\":\"One\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]],[{\"cardId\":44,\"color\":\"Red\",\"number\":\"Two\"},[{\"tag\":\"IsNumber\",\"contents\":\"Two\"},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}}]],[{\"cardId\":17,\"color\":\"Yellow\",\"number\":\"Four\"},[{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsColor\",\"contents\":\"White\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"One\"}},{\"tag\":\"Not\",\"contents\":{\"tag\":\"IsNumber\",\"contents\":\"Two\"}}]]]},\"deck\":[{\"cardId\":10,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":40,\"color\":\"Red\",\"number\":\"One\"},{\"cardId\":5,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":29,\"color\":\"Green\",\"number\":\"Five\"},{\"cardId\":18,\"color\":\"Yellow\",\"number\":\"Four\"},{\"cardId\":25,\"color\":\"Green\",\"number\":\"Three\"},{\"cardId\":22,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":9,\"color\":\"White\",\"number\":\"Five\"},{\"cardId\":21,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":49,\"color\":\"Red\",\"number\":\"Five\"},{\"cardId\":20,\"color\":\"Green\",\"number\":\"One\"},{\"cardId\":37,\"color\":\"Blue\",\"number\":\"Four\"},{\"cardId\":36,\"color\":\"Blue\",\"number\":\"Three\"},{\"cardId\":15,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":31,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":0,\"color\":\"White\",\"number\":\"One\"},{\"cardId\":13,\"color\":\"Yellow\",\"number\":\"Two\"},{\"cardId\":46,\"color\":\"Red\",\"number\":\"Three\"},{\"cardId\":3,\"color\":\"White\",\"number\":\"Two\"},{\"cardId\":6,\"color\":\"White\",\"number\":\"Three\"},{\"cardId\":39,\"color\":\"Blue\",\"number\":\"Five\"},{\"cardId\":16,\"color\":\"Yellow\",\"number\":\"Three\"},{\"cardId\":34,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":48,\"color\":\"Red\",\"number\":\"Four\"},{\"cardId\":23,\"color\":\"Green\",\"number\":\"Two\"},{\"cardId\":12,\"color\":\"Yellow\",\"number\":\"One\"},{\"cardId\":14,\"color\":\"Yellow\",\"number\":\"Two\"}],\"playedCards\":{\"White\":[{\"cardId\":2,\"color\":\"White\",\"number\":\"One\"}],\"Yellow\":[{\"cardId\":11,\"color\":\"Yellow\",\"number\":\"One\"}],\"Blue\":[{\"cardId\":33,\"color\":\"Blue\",\"number\":\"Two\"},{\"cardId\":32,\"color\":\"Blue\",\"number\":\"One\"}],\"Red\":[{\"cardId\":43,\"color\":\"Red\",\"number\":\"Two\"},{\"cardId\":41,\"color\":\"Red\",\"number\":\"One\"}]},\"discardedCards\":[{\"cardId\":30,\"color\":\"Blue\",\"number\":\"One\"},{\"cardId\":1,\"color\":\"White\",\"number\":\"One\"}],\"hints\":5,\"fuckups\":1,\"lastPlayer\":null}"

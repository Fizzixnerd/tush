{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Tush.Display where

import ClassyPrelude
import qualified Text.PrettyPrint as PP
import Data.Data
import qualified Graphics.Vty as V
import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.List as B
import Control.Lens

import qualified Language.Tush.Syntax as S
import Language.Tush

data TushiName = PromptCursor
               | PromptViewport
               | PromptEditor
               | OutputCursor
               | OutputViewport
               | OutputList
               | ErrorCursor
               | ErrorViewport
               | InteractiveCursor
               | InteractiveViewport
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Tushi = Tushi
  { _tushiPromptEditor :: B.Editor Text TushiName
  , _tushiFocusRing :: B.FocusRing TushiName
  , _tushiOutputList :: B.List TushiName Text
  } deriving (Typeable, Generic)

makeLenses ''Tushi

drawUI :: Tushi -> [B.Widget TushiName]
drawUI t = [ui]
  where
    pe = B.withFocusRing (t^.tushiFocusRing) (B.renderEditor (B.txt . unlines)) (t^.tushiPromptEditor)
    l = B.withFocusRing (t^.tushiFocusRing) (B.renderList (\_ val -> B.txtWrap val)) (t^.tushiOutputList)
    ui = B.txt "λ " B.<+> (B.hLimit 80 $ B.vLimit 1 pe) B.<=>
         B.txt " " B.<=>
         B.txt "Press Esc to quit." B.<=>
         l

appEvent :: Tushi -> B.BrickEvent TushiName e -> B.EventM TushiName (B.Next Tushi)
appEvent t (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> B.halt t
    V.EvKey (V.KChar 'o') [V.MCtrl] -> B.continue $ t & tushiFocusRing %~ B.focusNext
    V.EvKey V.KEnter [] -> do
      let [expression] = B.getEditContents $ t ^. tushiPromptEditor
      value <- liftIO $ evalEText expression
      textValue <- liftIO $ S.tshow value
      let t' = t & tushiPromptEditor .~ emptyPrompt
                 & tushiOutputList %~ B.listInsert 0 (fromString $ PP.render textValue)
      B.continue t'
    _ -> do
      next <- case B.focusGetCurrent (t^.tushiFocusRing) of
        Just PromptEditor -> B.handleEventLensed t tushiPromptEditor B.handleEditorEvent ev
        Just OutputList -> B.handleEventLensed t tushiOutputList B.handleListEvent ev
        Nothing -> return t
        _ -> error "Unreachable: All focus ring possibility exhausted in `appEvent'."
      B.continue next
appEvent t _ = B.continue t

tushiMap :: B.AttrMap
tushiMap = B.attrMap V.defAttr
           [ (B.editAttr, V.white `B.on` V.blue)
           , (B.editFocusedAttr, V.black `B.on` V.yellow)
           ]

appCursor :: Tushi -> [B.CursorLocation TushiName] -> Maybe (B.CursorLocation TushiName)
appCursor = B.focusRingCursor (^.tushiFocusRing)

emptyPrompt :: B.Editor Text TushiName
emptyPrompt = B.editorText PromptEditor (Just 1) ""

initTushi :: Tushi
initTushi = Tushi
  { _tushiPromptEditor = emptyPrompt
  , _tushiFocusRing = B.focusRing [PromptEditor, OutputList]
  , _tushiOutputList = B.list OutputList empty 1
  }

tushiApp :: B.App Tushi e TushiName
tushiApp = B.App
  { B.appDraw = drawUI
  , B.appChooseCursor = appCursor
  , B.appHandleEvent = appEvent
  , B.appStartEvent = return
  , B.appAttrMap = const tushiMap
  }

someFunc :: IO ()
someFunc = do
  void $ B.defaultMain tushiApp initTushi
  putStrLn "goodbye."

-- someFunc :: IO ()
-- someFunc = do
--   cfg <- standardIOConfig
--   vty <- mkVty cfg
--   let line0 = string (defAttr `withForeColor` green) "first line"
--       line1 = string (defAttr `withBackColor` blue) "second line"
--       img = line0 <-> line1
--       pic = picForImage img
--   update vty pic
--   e <- nextEvent vty
--   shutdown vty
--   putStrLn $ fromString $ printf "Final event was: %s" (show e)

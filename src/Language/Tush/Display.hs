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
import qualified Data.Vector as V
import Control.Lens
import Text.Printf

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
               | InteractiveDisplayList
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Tushi = Tushi
  { _tushiPromptEditor :: B.Editor Text TushiName
  , _tushiFocusRing :: B.FocusRing TushiName
  , _tushiOutputList :: B.List TushiName Text
  , _tushiInteractiveDisplay :: B.List TushiName (B.Widget TushiName)
  } deriving (Typeable, Generic)

makeLenses ''Tushi

drawUI :: Tushi -> [B.Widget TushiName]
drawUI t = [ui]
  where
    pe = B.withFocusRing (t^.tushiFocusRing) (B.renderEditor (B.txt . unlines))
         (t^.tushiPromptEditor)
    lst = B.withFocusRing (t^.tushiFocusRing) (B.renderList (\_ val -> B.txtWrap val))
          (t^.tushiOutputList)
    interactiveDisplay = B.withFocusRing (t^.tushiFocusRing)
                         (B.renderList (\_ w -> w)) (t ^. tushiInteractiveDisplay)
    ui = B.hLimit 80 (B.txt "λ " B.<+> B.vLimit 1 pe B.<=>
                      B.txt " " B.<=>
                      B.txt "Press Esc to quit." B.<=>
                      lst) B.<+> interactiveDisplay

appEvent :: Tushi -> B.BrickEvent TushiName e -> B.EventM TushiName (B.Next Tushi)
appEvent t (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> B.halt t
    V.EvKey (V.KChar 'o') [V.MCtrl] -> B.continue $ t & tushiFocusRing %~ B.focusNext
    V.EvKey V.KEnter [] -> do
      let [expression] = B.getEditContents $ t ^. tushiPromptEditor
          -- FIXME: This could be Left!
          Right (S.TushTokenStream lexed) = lex expression
      -- FIXME: evalEText can throw.
      value <- liftIO $ evalEText expression
      textValue <- liftIO $ S.tshow value
      -- Update the OutputList and the Prompt; clear the InteractiveDisplay.
      let t' = t & tushiPromptEditor .~ emptyPrompt
                 & tushiOutputList %~ B.listInsert 0 (fromString $ PP.render textValue)
                 & tushiOutputList %~ B.listMoveUp
                 & tushiInteractiveDisplay %~ B.listClear
      -- Update the InteractiveDisplay based on last command run.
      mainInteractiveWidget <- case lexed V.! 0 of
        S.DebugToken { S._dtToken = lexed' } ->
          case lexed' of
            (S.TIdentifier "cd") -> do
              dirContents <- liftIO $ evalEText "ls ./"
              docContents <- liftIO $ S.tshow dirContents
              let textContents = fromString $ PP.render docContents
              return $ B.txtWrap textContents
            S.TPath p S.PATH _  -> do
              let cmdName = V.last p
              S.ETuple (S.TushTuple ( (S.EInt ec)
                                    , (S.ETuple (S.TushTuple ( (S.EString (S.TushString manContents))
                                                             , _))))) <- liftIO $ evalEText $ fromString $ printf "!/man [\"%s\"]" (unpack cmdName)
              if ec == 0
                then return $ B.txt manContents
                else return B.emptyWidget
            _ -> return B.emptyWidget
      let t'' = t' & tushiInteractiveDisplay %~ B.listInsert 0 mainInteractiveWidget
      B.continue t''
    _ -> do
      next <- case B.focusGetCurrent (t^.tushiFocusRing) of
        Just PromptEditor -> B.handleEventLensed t tushiPromptEditor B.handleEditorEvent ev
        Just OutputList -> B.handleEventLensed t tushiOutputList B.handleListEvent ev
        Just InteractiveDisplayList -> B.handleEventLensed t tushiInteractiveDisplay B.handleListEvent ev
        Nothing -> return t
        _ -> error "Unreachable: All focus ring possibility exhausted in `appEvent'."
      B.continue next
appEvent t _ = B.continue t

tushiMap :: B.AttrMap
tushiMap = B.attrMap V.defAttr
           [ (B.editAttr, V.white `B.on` V.blue)
           , (B.editFocusedAttr, V.black `B.on` V.yellow)
           , (B.listSelectedAttr, V.white `B.on` V.blue)
           , (B.listSelectedFocusedAttr, V.black `B.on` V.yellow)
           ]

appCursor :: Tushi -> [B.CursorLocation TushiName] -> Maybe (B.CursorLocation TushiName)
appCursor = B.focusRingCursor (^.tushiFocusRing)

emptyPrompt :: B.Editor Text TushiName
emptyPrompt = B.editorText PromptEditor (Just 1) ""

initTushi :: Tushi
initTushi = Tushi
  { _tushiPromptEditor = emptyPrompt
  , _tushiFocusRing = B.focusRing [PromptEditor, OutputList, InteractiveDisplayList]
  , _tushiOutputList = B.list OutputList empty 1
  , _tushiInteractiveDisplay = B.list InteractiveDisplayList empty 1
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

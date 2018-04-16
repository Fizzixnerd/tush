{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified System.Process.Typed as P
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Exit
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
  case B.focusGetCurrent (t^.tushiFocusRing) of
    Nothing -> B.continue t
    Just OutputList -> case ev of
      V.EvKey V.KEsc [] -> B.halt t
      V.EvKey (V.KChar 'o') [V.MCtrl] -> B.continue $ t & tushiFocusRing %~ B.focusNext
      _ -> do
        next <- B.handleEventLensed t tushiOutputList B.handleListEvent ev
        B.continue next
    Just InteractiveDisplayList -> case ev of
      V.EvKey V.KEsc [] -> B.halt t
      V.EvKey (V.KChar 'o') [V.MCtrl] -> B.continue $ t & tushiFocusRing %~ B.focusNext
      _ -> do
        next <- B.handleEventLensed t tushiInteractiveDisplay B.handleListEvent ev
        B.continue next
    Just PromptEditor ->
      case ev of
        V.EvKey V.KEsc [] -> B.halt t
        V.EvKey (V.KChar 'o') [V.MCtrl] -> B.continue $ t & tushiFocusRing %~ B.focusNext
        V.EvKey V.KEnter [] -> do
          -- FIXME: This is irrefutable, but is it!?
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
          interactiveWidgets <- case lexed V.! 0 of
            S.DebugToken { S._dtToken = lexed' } ->
              case lexed' of
                (S.TIdentifier "cd") -> do
                  dirContents <- liftIO $ evalEText "ls ./"
                  docContents <- liftIO $ S.tshow dirContents
                  let textContents = fromString $ PP.render docContents
                  return $ singleton $ B.txtWrap textContents
                S.TPath p S.PATH _  -> do
                  let cmdName = V.last p
                  (ec, manContents, _) <- liftIO $ P.readProcess $ P.shell $ printf "MANWIDTH=80 man --nh '%s' | col -bx" (unpack cmdName)
                  if ec == ExitSuccess
                    then return $ fromList $ B.txt <$> (\case
                                                           "" -> " "
                                                           x -> x) <$>
                         (lines $ fromString $ BS.unpack manContents)
                    else return empty
                _ -> return empty
          let t'' = t' & tushiInteractiveDisplay . B.listElementsL .~ interactiveWidgets
                       & tushiInteractiveDisplay . B.listSelectedL ?~ 0
          B.continue t''
        _ -> do
          next <- B.handleEventLensed t tushiPromptEditor B.handleEditorEvent ev
          B.continue next
    _ -> error "Unreachable: All focus ring possibility exhausted in `appEvent'."
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

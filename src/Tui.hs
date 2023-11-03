{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as Ne
import Cursor.Simple.List.NonEmpty
import System.Exit
import Control.Monad.IO.Class
import Control.Monad

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Border
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState 
  {
    tuiStatePaths :: NonEmptyCursor POC
  } deriving (Show, Eq)

data POC = 
  File FilePath 
  | Directory FilePath
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)]
    }

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  contents' <-
    forM contents $ \fp -> do
      e <- doesFileExist fp
      pure $
        if e
          then File fp
          else Directory fp
  case Ne.nonEmpty contents of
    Nothing -> die "There is no contents!"
    Just ne -> pure TuiState {tuiStatePaths = makeNonEmptyCursor ne}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let nec = tuiStatePaths ts
  in [ border $ 
    vBox $ 
      concat
        [
          map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
          , [drawPath True $ nonEmptyCursorCurrent nec]
          ,map (drawPath False) $ nonEmptyCursorNext nec
        ]
  ]
drawPath :: Bool -> POC -> Widget n
drawPath b poc = 
  (if b
        then withAttr "selected"
        else id) $
  case poc of
    File fp -> withAttr "file" $ str fp
    Directory fp -> withAttr "directory" $ str fp


handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KDown [] -> do
          let nec = tuiStatePaths s
          case nonEmptyCursorSelectNext nec of
                Nothing -> continue s
                Just nec' -> continue $ s {tuiStatePaths = nec'}
        EvKey KUp [] -> do
          let nec = tuiStatePaths s
          case nonEmptyCursorSelectPrev nec of
                Nothing -> continue s
                Just nec' -> continue $ s {tuiStatePaths = nec'}
        EvKey KEnter [] -> do
          let fp = nonEmptyCursorCurrent $ tuiStatePaths s
          case fp of
            File _ -> continue s
            Directory fp -> do
                  liftIO $ setCurrentDirectory fp
                  s' <- liftIO buildInitialState
                  continue s'
        _ -> continue s
    _ -> continue s

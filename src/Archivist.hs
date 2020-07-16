{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Archivist where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad
import Control.Monad.IO.Class
import Cursor.Brick.List.NonEmpty
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events
import Path
import Path.IO

archivist :: IO ()
archivist = do
  home <- getHomeDir
  nurseryDir <- resolveDir home ".archivist"
  let sets = Settings {setNurseryDir = nurseryDir}
  initialState <- buildInitialState sets
  endState <- defaultMain (tuiApp sets) initialState
  print endState

data Settings
  = Settings
      { setNurseryDir :: Path Abs Dir
      }

data State
  = State
      { stateNursery :: Maybe (NonEmptyCursor (Path Rel File)),
        stateButtons :: NonEmptyCursor ArchivistButton,
        stateSelection :: Selection
      }
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

data ArchivistButton
  = ButtonScan
  | ButtonCombine
  deriving (Show, Eq, Ord, Enum, Bounded)

data Selection = NurserySelection | ButtonSelection
  deriving (Show, Eq)

data Selected = MayBeSelected | NotSelected
  deriving (Show, Eq)

instance Semigroup Selected where
  MayBeSelected <> MayBeSelected = MayBeSelected
  _ <> _ = NotSelected

tuiApp :: Settings -> App State e ResourceName
tuiApp sets =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent sets,
      appStartEvent = pure,
      appAttrMap = const $ attrMap defAttr [("selected", fg green)]
    }

select :: Selected -> Widget n -> Widget n
select = \case
  MayBeSelected -> withDefAttr selectedAttr
  _ -> id

selectedIf :: Bool -> Selected
selectedIf True = MayBeSelected
selectedIf False = NotSelected

selectedAttr :: AttrName
selectedAttr = "selected"

buildInitialState :: Settings -> IO State
buildInitialState Settings {..} = do
  mfs <- forgivingAbsence $ snd <$> listDirRecurRel setNurseryDir
  pure
    State
      { stateNursery = makeNonEmptyCursor <$> (mfs >>= NE.nonEmpty),
        stateButtons = makeNonEmptyCursor $ NE.fromList [minBound .. maxBound],
        stateSelection = ButtonSelection
      }

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  [ vBox
      [ padBottom Max $ centerLayer $ nurseryWidget (selectedIf (stateSelection == NurserySelection)) stateNursery,
        hBorder,
        vLimit 5 $ buttonsWidget (selectedIf (stateSelection == ButtonSelection)) stateButtons
      ]
  ]

nurseryWidget :: Selected -> Maybe (NonEmptyCursor (Path Rel File)) -> Widget n
nurseryWidget s = maybe (str "No files") $ verticalNonEmptyCursorWidget (go NotSelected) (go s) (go NotSelected)
  where
    go :: Selected -> Path Rel File -> Widget n
    go s = select s . str . show

buttonsWidget :: Selected -> NonEmptyCursor ArchivistButton -> Widget n
buttonsWidget s = horizontalNonEmptyCursorWidget (go NotSelected) (go s) (go NotSelected)
  where
    go :: Selected -> ArchivistButton -> Widget n
    go s = padAll 1 . border . select s . str . show

handleTuiEvent :: Settings -> State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent sets s e =
  case e of
    VtyEvent vtye ->
      let setSelection sel = continue $ s {stateSelection = sel}
       in case stateSelection s of
            NurserySelection ->
              let modMNurseryM func = continue $ s {stateNursery = func (stateNursery s)}
                  modNursery func = modMNurseryM $ fmap func
                  modNurseryM func = modNursery $ \nec -> fromMaybe nec $ func nec
               in case vtye of
                    EvKey (KChar 'q') [] -> halt s
                    EvKey (KChar 'r') [] -> refresh sets >>= continue
                    EvKey KDown [] -> modNurseryM nonEmptyCursorSelectNext
                    EvKey (KChar 'j') [] -> modNurseryM nonEmptyCursorSelectNext
                    EvKey KUp [] -> modNurseryM nonEmptyCursorSelectPrev
                    EvKey (KChar 'k') [] -> modNurseryM nonEmptyCursorSelectPrev
                    EvKey (KChar '\t') [] -> setSelection ButtonSelection
                    _ -> continue s
            ButtonSelection ->
              let modButtons func = continue $ s {stateButtons = func (stateButtons s)}
                  modButtonsM func = modButtons $ \nec -> fromMaybe nec $ func nec
               in case vtye of
                    EvKey (KChar 'q') [] -> halt s
                    EvKey (KChar 'r') [] -> refresh sets >>= continue
                    EvKey KLeft [] -> modButtonsM nonEmptyCursorSelectPrev
                    EvKey (KChar 'h') [] -> modButtonsM nonEmptyCursorSelectPrev
                    EvKey KRight [] -> modButtonsM nonEmptyCursorSelectNext
                    EvKey (KChar 'l') [] -> modButtonsM nonEmptyCursorSelectNext
                    EvKey (KChar '\t') [] -> setSelection NurserySelection
                    _ -> continue s
    _ -> continue s

refresh :: Settings -> EventM n State
refresh = liftIO . buildInitialState

doBatchScan :: Int -> IO ()
doBatchScan = undefined

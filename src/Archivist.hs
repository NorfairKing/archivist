{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Archivist where

import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Cursor.Brick.List.NonEmpty
import Cursor.Simple.List.NonEmpty
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Graphics.Vty as Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events
import Path
import Path.IO
import System.IO
import System.Process.Typed

archivist :: IO ()
archivist = do
  home <- getHomeDir
  nurseryDir <- resolveDir home ".archivist"
  chan <- newBChan 100
  let sets = Settings {setChan = chan, setNurseryDir = nurseryDir}
  initialState <- buildInitialState sets
  -- Start the app
  let builder = Vty.mkVty Vty.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) (tuiApp sets) initialState

data Settings
  = Settings
      { setChan :: BChan ArchivistEvent,
        setNurseryDir :: Path Abs Dir
      }

data State
  = State
      { stateNursery :: Maybe (NonEmptyCursor (Path Rel File)),
        stateButtons :: NonEmptyCursor ArchivistButton,
        stateSelection :: Selection,
        stateScanProcess :: Maybe ScanProcess
      }

data ScanProcess
  = ScanProcess
      { scanProcessChecker :: Async (),
        scanProcess :: Process () Handle Handle,
        scanProcessOutputSoFar :: ByteString
      }

data ArchivistEvent
  = EventCheckProcess
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

tuiApp :: Settings -> App State ArchivistEvent ResourceName
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
        stateSelection = ButtonSelection,
        stateScanProcess = Nothing
      }

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  concat
    [ [scanProcessWidget sp | sp <- maybeToList stateScanProcess],
      [ vBox
          [ padBottom Max $ padRight Max $ padAll 1 $ nurseryWidget (selectedIf (stateSelection == NurserySelection)) stateNursery,
            hBorder,
            vLimit 5 $ buttonsWidget (selectedIf (stateSelection == ButtonSelection)) stateButtons
          ]
      ]
    ]

nurseryWidget :: Selected -> Maybe (NonEmptyCursor (Path Rel File)) -> Widget n
nurseryWidget s = maybe (str "No files") $ verticalNonEmptyCursorWidget (go NotSelected) (go s) (go NotSelected)
  where
    go :: Selected -> Path Rel File -> Widget n
    go s rf =
      select s $
        hBox
          [ str $ case s of
              MayBeSelected -> "❯ "
              NotSelected -> "- ",
            str (fromRelFile rf)
          ]

buttonsWidget :: Selected -> NonEmptyCursor ArchivistButton -> Widget n
buttonsWidget s = horizontalNonEmptyCursorWidget (go NotSelected) (go s) (go NotSelected)
  where
    go :: Selected -> ArchivistButton -> Widget n
    go s = padAll 1 . border . select s . str . show

scanProcessWidget :: ScanProcess -> Widget n
scanProcessWidget ScanProcess {..} = centerLayer $ border $ padAll 1 $ txtWrap $ TE.decodeUtf8With TE.lenientDecode scanProcessOutputSoFar

handleTuiEvent :: Settings -> State -> BrickEvent n ArchivistEvent -> EventM n (Next State)
handleTuiEvent sets s be =
  case be of
    AppEvent ae -> case ae of
      EventCheckProcess -> checkProcess s
    VtyEvent vtye ->
      let setSelection sel = continue $ s {stateSelection = sel}
       in case stateSelection s of
            NurserySelection ->
              let modMNurseryM func = continue $ s {stateNursery = func (stateNursery s)}
                  modNursery func = modMNurseryM $ fmap func
                  modNurseryM func = modNursery $ \nec -> fromMaybe nec $ func nec
               in case vtye of
                    EvKey (KChar 'q') [] -> halt s
                    EvKey (KChar 'r') [] -> refreshNursery sets s >>= continue
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
                    EvKey (KChar 'r') [] -> refreshNursery sets s >>= continue
                    EvKey KLeft [] -> modButtonsM nonEmptyCursorSelectPrev
                    EvKey (KChar 'h') [] -> modButtonsM nonEmptyCursorSelectPrev
                    EvKey KRight [] -> modButtonsM nonEmptyCursorSelectNext
                    EvKey (KChar 'l') [] -> modButtonsM nonEmptyCursorSelectNext
                    EvKey (KChar '\t') [] -> setSelection NurserySelection
                    EvKey KEnter [] -> case nonEmptyCursorCurrent (stateButtons s) of
                      ButtonScan -> case stateScanProcess s of
                        Nothing -> do
                          sp <- liftIO (startBatchScan sets)
                          s' <- refreshNursery sets $ s {stateScanProcess = Just sp}
                          continue s'
                        _ -> continue s
                      _ -> continue s
                    _ -> continue s
    _ -> continue s

refreshNursery :: Settings -> State -> EventM n State
refreshNursery Settings {..} s = do
  mfs <- forgivingAbsence $ snd <$> listDirRecurRel setNurseryDir
  pure
    s
      { stateNursery = makeNonEmptyCursor <$> (mfs >>= NE.nonEmpty)
      }

-- TODO make sure that the process is stopped

startBatchScan :: Settings -> IO ScanProcess
startBatchScan Settings {..} = do
  let wd = setNurseryDir
      pc =
        setStdin nullStream
          $ setStdout createPipe
          $ setStderr createPipe
          $ setWorkingDir (fromAbsDir wd)
          $ proc
            "scanimage"
            [ "--batch=scan%d.pnm",
              "--batch-print",
              "-x",
              "210mm",
              "-y",
              "297mm",
              "-l 40",
              "--mode",
              "Black & White",
              "--resolution",
              "150",
              "--batch-start=1"
            ]
  ensureDir wd
  p <- startProcess pc
  c <- async $ processEventSender setChan p
  pure $ ScanProcess {scanProcessChecker = c, scanProcess = p, scanProcessOutputSoFar = ""}

processEventSender :: BChan ArchivistEvent -> Process () Handle Handle -> IO ()
processEventSender chan p = go
  where
    go = do
      mec <- getExitCode p
      case mec of
        Nothing -> do
          writeBChan chan EventCheckProcess
          threadDelay $ 100 * 1000
          go
        Just _ -> writeBChan chan EventCheckProcess -- The last one.

checkProcess :: State -> EventM n (Next State)
checkProcess s = case stateScanProcess s of
  Nothing -> continue s
  Just sp -> do
    let p = scanProcess sp
    sp' <- addFrom (getStdout p) sp
    sp'' <- addFrom (getStderr p) sp'
    let s' = s {stateScanProcess = Just sp''}
    tryToFinishScanProcess s' >>= continue
  where
    addFrom h sp = do
      msp' <- addPiece sp
      case msp' of
        Nothing -> pure sp
        Just sp' -> addFrom h sp'
      where
        addPiece sp = do
          out <- liftIO $ SB.hGetNonBlocking h chunkSize
          pure $
            if SB.null out
              then Nothing
              else Just $ sp {scanProcessOutputSoFar = scanProcessOutputSoFar sp <> out}
    chunkSize = 1024

tryToFinishScanProcess :: State -> EventM n State
tryToFinishScanProcess s = case stateScanProcess s of
  Nothing -> pure s
  Just sp -> do
    let p = scanProcess sp
    mec <- liftIO $ getExitCode p
    case mec of
      Nothing -> pure s
      Just _ -> do
        liftIO $ do
          void $ waitExitCode p
          void $ wait $ scanProcessChecker sp
        pure $ s {stateScanProcess = Nothing}

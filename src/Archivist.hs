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
import Cursor.Brick.Text
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Graphics.Vty as Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events
import Lens.Micro
import Path
import Path.IO
import System.Exit
import System.IO
import System.Process.Typed

archivist :: IO ()
archivist = do
  home <- getHomeDir
  nurseryDir <- resolveDir home ".archivist"
  outDir <- resolveDir home "scans"
  chan <- newBChan 100
  let sets = Settings {setChan = chan, setNurseryDir = nurseryDir, setOutDir = outDir}
  initialState <- buildInitialState sets
  -- Start the app
  let builder = Vty.mkVty Vty.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) (tuiApp sets) initialState

data Settings
  = Settings
      { setChan :: BChan ArchivistEvent,
        setNurseryDir :: Path Abs Dir,
        setOutDir :: Path Abs Dir
      }

data State
  = State
      { stateNursery :: Maybe (NonEmptyCursor (Path Rel File)),
        stateSelectedFiles :: Maybe (NonEmpty (Path Rel File)),
        stateButtons :: NonEmptyCursor ArchivistButton,
        stateMode :: ArchivistMode,
        stateSelection :: Selection,
        stateScanProcess :: Maybe ScanProcess,
        stateError :: Maybe String
      }

data ScanProcess
  = ScanProcess
      { scanProcessChecker :: Async (),
        scanProcess :: Process () Handle Handle,
        scanProcessOutputSoFar :: ByteString
      }

data ArchivistEvent
  = EventCheckProcess
  | EventEndProcess
  deriving (Show, Eq)

data ResourceName
  = ButtonTextCursor
  | OutputViewport
  deriving (Show, Eq, Ord)

data ArchivistButton
  = ButtonScan TextCursor
  | ButtonCombine TextCursor
  deriving (Show, Eq)

data ArchivistMode = InsertMode | NormalMode
  deriving (Show, Eq)

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
      appAttrMap =
        const $
          attrMap
            defAttr
            [ (selectedAttr, fg green),
              (pdfAttr, fg blue),
              (otherAttr, fg yellow),
              (errAttr, fg red)
            ]
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

pdfAttr :: AttrName
pdfAttr = "pdf"

otherAttr :: AttrName
otherAttr = "other"

errAttr :: AttrName
errAttr = "error"

buildInitialState :: Settings -> IO State
buildInitialState Settings {..} = do
  mfs <- forgivingAbsence $ snd <$> listDirRecurRel setNurseryDir
  let mnec = makeNonEmptyCursor <$> (mfs >>= NE.nonEmpty)
  pure
    State
      { stateNursery = mnec,
        stateSelectedFiles = Nothing,
        stateButtons = makeNonEmptyCursor $ NE.fromList [ButtonScan emptyTextCursor, ButtonCombine emptyTextCursor],
        stateMode = NormalMode,
        stateSelection =
          if isJust mnec
            then NurserySelection
            else ButtonSelection,
        stateScanProcess = Nothing,
        stateError = Nothing
      }

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  [ vBox $
      concat
        [ [ padBottom Max $ padRight Max $ padAll 1 $ nurseryWidget (selectedIf (stateSelection == NurserySelection)) stateNursery stateSelectedFiles,
            hBorder
          ],
          [vLimit 3 $ padAll 1 $ withDefAttr errAttr $ str e | e <- maybeToList stateError],
          [vLimitPercent 25 $ scanProcessWidget sp | sp <- maybeToList stateScanProcess],
          [vLimit 5 $ buttonsWidget (selectedIf (stateSelection == ButtonSelection)) stateMode stateButtons],
          [hBox [str (show stateSelection), str " ", str (show stateMode)] | debug],
          [strWrap (show stateButtons) | debug]
        ]
  ]

debug :: Bool
debug = False

nurseryWidget :: Selected -> Maybe (NonEmptyCursor (Path Rel File)) -> Maybe (NonEmpty (Path Rel File)) -> Widget n
nurseryWidget s nec ne = case mRightPart ne of
  Nothing -> padLeftRight 1 leftPart
  Just rightPart -> hBox [padLeftRight 1 $ hLimitPercent 75 $ padRight Max $ leftPart, vBorder, padLeftRight 1 rightPart]
  where
    leftPart = maybe (str "No files") (verticalNonEmptyCursorWidget (go NotSelected) (go s) (go NotSelected)) nec
    mRightPart = fmap $ \ne -> vBox $ map (str . fromRelFile) (NE.toList ne)
    go :: Selected -> Path Rel File -> Widget n
    go s rf =
      select s $
        hBox
          [ str $ case s of
              MayBeSelected -> "❯ "
              NotSelected -> "- ",
            case fileExtension rf of
              Just ".pdf" -> withDefAttr pdfAttr $ str $ fromRelFile rf
              _ -> withDefAttr otherAttr $ str $ fromRelFile rf
          ]

buttonsWidget :: Selected -> ArchivistMode -> NonEmptyCursor ArchivistButton -> Widget ResourceName
buttonsWidget s m = horizontalNonEmptyCursorWidget (go NotSelected) (go s) (go NotSelected)
  where
    go :: Selected -> ArchivistButton -> Widget ResourceName
    go s ab = padAll 1 . border . select s $ case ab of
      ButtonScan tc ->
        hBox
          [ str "Scan:",
            padLeftRight 1 $
              case s of
                MayBeSelected -> case m of
                  NormalMode -> textCursorWidget tc
                  InsertMode -> selectedTextCursorWidget ButtonTextCursor tc
                NotSelected -> textCursorWidget tc
          ]
      ButtonCombine tc ->
        hBox
          [ str "Combine:",
            padLeftRight 1 $
              case s of
                MayBeSelected -> case m of
                  NormalMode -> textCursorWidget tc
                  InsertMode -> selectedTextCursorWidget ButtonTextCursor tc
                NotSelected -> textCursorWidget tc
          ]

scanProcessWidget :: ScanProcess -> Widget ResourceName
scanProcessWidget ScanProcess {..} =
  centerLayer $ border $ padAll 1 $ viewport OutputViewport Vertical $
    let ls = T.lines $ TE.decodeUtf8With TE.lenientDecode scanProcessOutputSoFar
        go [] = []
        go [t] = [visible t]
        go (t : ts) = t : go ts
     in vBox $ go $ map txtWrap ls

handleTuiEvent :: Settings -> State -> BrickEvent n ArchivistEvent -> EventM n (Next State)
handleTuiEvent sets s be =
  let err str = continue $ s {stateError = Just str}
   in case be of
        AppEvent ae -> case ae of
          EventCheckProcess -> checkProcess sets s
          EventEndProcess -> do
            liftIO $ convertToPdfs (setNurseryDir sets)
            refreshNursery sets (s {stateSelection = NurserySelection, stateMode = NormalMode}) >>= continue
        VtyEvent vtye ->
          let setSelection sel = continue $ s {stateSelection = sel}
              setMode m = continue $ s {stateMode = m}
           in case stateSelection s of
                NurserySelection ->
                  let modMNurseryM func = continue $ s {stateNursery = func (stateNursery s)}
                      modNursery func = modMNurseryM $ fmap func
                      modNurseryDOU func = modMNurseryM $ \mnec -> mnec >>= (dullDelete . func)
                      modNurseryM func = modNursery $ \nec -> fromMaybe nec $ func nec
                      mcf = nonEmptyCursorCurrent <$> stateNursery s
                   in case vtye of
                        EvKey (KChar 'q') [] -> halt s
                        EvKey (KChar 's') [] -> setSelection ButtonSelection
                        EvKey (KChar 'c') [] -> setSelection ButtonSelection
                        EvKey (KChar 'p') [] -> do
                          liftIO $ convertToPdfs (setNurseryDir sets)
                          refreshNursery sets s >>= continue
                        EvKey (KChar 'r') [] -> refreshNursery sets s >>= continue
                        EvKey KDown [] -> modNurseryM nonEmptyCursorSelectNext
                        EvKey (KChar 'j') [] -> modNurseryM nonEmptyCursorSelectNext
                        EvKey KUp [] -> modNurseryM nonEmptyCursorSelectPrev
                        EvKey (KChar 'k') [] -> modNurseryM nonEmptyCursorSelectPrev
                        EvKey (KChar '\t') [] -> setSelection ButtonSelection
                        EvKey (KChar 'D') [] -> case mcf of
                          Nothing -> err "No file to delete"
                          Just rf -> do
                            let af = setNurseryDir sets </> rf
                            ignoringAbsence $ removeFile af
                            modNurseryDOU nonEmptyCursorRemoveElem
                        EvKey (KChar 'e') [] -> case mcf of
                          Nothing -> err "No file to move out"
                          Just rf -> do
                            let inf = setNurseryDir sets </> rf
                                outf = setOutDir sets </> rf
                            ensureDir $ parent outf
                            renameFile inf outf
                            modNurseryDOU nonEmptyCursorRemoveElem
                        EvKey (KChar 'o') [] -> case mcf of
                          Nothing -> err "No file to open"
                          Just rf -> do
                            liftIO $ openPdfFile $ setNurseryDir sets </> rf
                            continue s
                        EvKey KEsc [] -> continue $ s {stateSelectedFiles = Nothing}
                        EvKey KEnter [] ->
                          case mcf of
                            Nothing -> err "No selected files."
                            Just cf ->
                              if fileExtension cf == Just ".pdf"
                                then continue $ case stateSelectedFiles s of
                                  Nothing -> s {stateSelectedFiles = Just $ cf :| []}
                                  Just ne ->
                                    if cf `elem` NE.toList ne
                                      then s {stateSelectedFiles = NE.nonEmpty $ NE.filter (/= cf) ne}
                                      else s {stateSelectedFiles = Just $ ne <> (cf :| [])}
                                else err "Cannot select a non-pdf file."
                        _ -> continue s
                ButtonSelection ->
                  let bs = stateButtons s
                      cb = nonEmptyCursorCurrent bs
                      modButtons func = continue $ s {stateButtons = func bs}
                      modButtonsM func = modButtons $ \nec -> fromMaybe nec $ func nec
                   in case stateMode s of
                        NormalMode -> case vtye of
                          EvKey (KChar 'q') [] -> halt s
                          EvKey (KChar 'r') [] -> refreshNursery sets s >>= continue
                          EvKey KLeft [] -> modButtonsM nonEmptyCursorSelectPrev
                          EvKey (KChar 'h') [] -> modButtonsM nonEmptyCursorSelectPrev
                          EvKey KRight [] -> modButtonsM nonEmptyCursorSelectNext
                          EvKey (KChar 'l') [] -> modButtonsM nonEmptyCursorSelectNext
                          EvKey (KChar '\t') [] -> setSelection NurserySelection
                          EvKey (KChar 'i') [] -> setMode InsertMode
                          EvKey (KChar 'a') [] -> setMode InsertMode
                          _ -> continue s
                        InsertMode ->
                          let handleTextCursor b tc te =
                                let modTC func = do
                                      let b' = b $ func tc
                                      modButtons (nonEmptyCursorElemL .~ b')
                                    modTCM func = modTC $ \tc -> fromMaybe tc $ func tc
                                    modTCMDOU func = modTCM $ dullMDelete . func
                                 in case vtye of
                                      EvKey KEsc _ -> setMode NormalMode
                                      EvKey KLeft _ -> modTCM textCursorSelectPrev
                                      EvKey KRight _ -> modTCM textCursorSelectNext
                                      EvKey KBS _ -> modTCMDOU textCursorRemove
                                      EvKey KDel _ -> modTCMDOU textCursorDelete
                                      EvKey (KChar '\t') [] -> setSelection NurserySelection
                                      EvKey (KChar c) _ -> modTCM $ textCursorInsert c
                                      _ -> continue s
                           in case cb of
                                ButtonScan tc -> case vtye of
                                  EvKey KEnter [] -> case stateScanProcess s of
                                    Nothing -> do
                                      case parseRelFile $ T.unpack $ rebuildTextCursor tc of
                                        Nothing -> err "Not a valid filename to scan."
                                        Just rf ->
                                          if anyInTheWay (stateNursery s) rf
                                            then err "There are already files in the way."
                                            else do
                                              sp <- liftIO (startBatchScan sets rf)
                                              s' <- refreshNursery sets $ s {stateScanProcess = Just sp}
                                              continue s'
                                    _ -> err "A scan has already started"
                                  _ -> handleTextCursor ButtonScan tc vtye
                                ButtonCombine tc ->
                                  case vtye of
                                    EvKey KEnter [] ->
                                      case stateSelectedFiles s of
                                        Nothing -> err "No selected files to combine"
                                        Just ne ->
                                          case parseRelFile (T.unpack (rebuildTextCursor tc)) >>= replaceExtension ".pdf" of
                                            Nothing -> err "Not a valid file to combine to"
                                            Just rf -> do
                                              liftIO $ combineFiles sets rf ne
                                              refreshNursery sets s >>= continue
                                    _ -> handleTextCursor ButtonCombine tc vtye
        _ -> continue s

refreshNursery :: Settings -> State -> EventM n State
refreshNursery Settings {..} s = do
  mfs <- forgivingAbsence $ snd <$> listDirRecurRel setNurseryDir
  pure
    s
      { stateNursery = makeNonEmptyCursor <$> (mfs >>= NE.nonEmpty)
      }

-- TODO make sure that the process is stopped

anyInTheWay :: Maybe (NonEmptyCursor (Path Rel File)) -> Path Rel File -> Bool
anyInTheWay nursery rf =
  let fs = maybe [] (NE.toList . rebuildNonEmptyCursor) nursery
   in any (\f -> fromRelFile rf `isInfixOf` fromRelFile f) fs

startBatchScan :: Settings -> Path Rel File -> IO ScanProcess
startBatchScan Settings {..} rf = do
  let wd = setNurseryDir
      pc =
        setStdin nullStream
          $ setStdout createPipe
          $ setStderr createPipe
          $ setWorkingDir (fromAbsDir wd)
          $ proc
            "scanimage"
            [ "--batch=" <> fromRelFile rf <> "%d.pnm",
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

checkProcess :: Settings -> State -> EventM n (Next State)
checkProcess sets s = case stateScanProcess s of
  Nothing -> continue s
  Just sp -> do
    let p = scanProcess sp
    sp' <- addFrom (getStdout p) sp
    sp'' <- addFrom (getStderr p) sp'
    let s' = s {stateScanProcess = Just sp''}
    tryToFinishScanProcess sets s' >>= continue
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

tryToFinishScanProcess :: Settings -> State -> EventM n State
tryToFinishScanProcess Settings {..} s = case stateScanProcess s of
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
          writeBChan setChan EventEndProcess
        pure $ s {stateScanProcess = Nothing}

openPdfFile :: Path Abs File -> IO ()
openPdfFile f = withProcessWait (setStderr nullStream $ setStdout nullStream $ setStdin nullStream $ proc "evince" [fromAbsFile f]) $ const $ pure ()

convertToPdfs :: Path Abs Dir -> IO ()
convertToPdfs dir = do
  fs <- snd <$> listDirRecur dir
  let pdfs = filter (\f -> fileExtension f /= Just ".pdf") fs
  mapM_ convertFile pdfs

convertFile :: Path Abs File -> IO ()
convertFile inf = do
  case replaceExtension ".pdf" inf of
    Nothing -> pure ()
    Just outf -> do
      ec <- runProcess $ proc "convert" [fromAbsFile inf, fromAbsFile outf]
      case ec of
        ExitSuccess -> removeFile inf
        ExitFailure _ -> pure ()

combineFiles :: Settings -> Path Rel File -> NonEmpty (Path Rel File) -> IO ()
combineFiles Settings {..} outFile inFiles = do
  let outAbsFile = setNurseryDir </> outFile
      inAbsFiles = map (setNurseryDir </>) $ NE.toList inFiles
  case inAbsFiles of
    [inAbsFile] -> copyFile inAbsFile outAbsFile
    _ -> void $ runProcess $ proc "pdfunite" $ map fromAbsFile inAbsFiles ++ [fromAbsFile outAbsFile]

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Game.Horca.UI.Brick.Editor where

import           Game.Horca.Types

import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic as ZG

import qualified Graphics.Vty as V
import           Graphics.Vty.Attributes

import           Brick
import qualified Brick.BChan as BC
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import qualified Brick.Widgets.Edit as E

import           Control.Monad (forever)

import           Control.Concurrent
import           Control.Concurrent.Async (Async, async, waitEither)
import           Control.Concurrent.STM (atomically,TChan,  readTChan, dupTChan, writeTChan, newTChan)

import           Data.Maybe (fromMaybe)

import           Lens.Micro ((+~), (-~), (^.), (&), (.~), (%~), over)
import           Lens.Micro.TH

import qualified Data.Vector as Vec


data HorcaEvent = TickEvent | TickHideIndicator | HorcaEvent
  deriving (Eq, Ord, Show, Read)

type BoardArrayRowType = T.Text
type BoardArrayType = Z.TextZipper BoardArrayRowType



data UIComponentName = BPMField | OpsCanvas |  OpsCanvasViewPort
  deriving (Eq, Ord, Show)

type SeqEditor = E.Editor BoardArrayRowType UIComponentName

data BPMInfo =
  BPMInfo {
    _bpm :: BPM,
    _bpmChan :: TChan BPM,
    _tickBool :: Bool,
    _editor :: SeqEditor,
    _exprVar :: TChan (Z.TextZipper BoardArrayRowType)
    }
makeLenses ''BPMInfo


gridBgAttr :: AttrName
gridBgAttr = attrName "edit"

theMap :: AttrMap
theMap = attrMap globalDefault
    [ (gridBgAttr,               brightBlack `on` black)
    , (attrName "canvasCursor",             bg yellow)
    ]


globalDefault :: Attr
globalDefault = white `on` black

renderBPM i = str $ show $ i ^. bpm

renderTick i = str $ if i ^. tickBool then "*" else " "

editorContentL = editor . E.editContentsL

renderCursorPosition i =  str $ show $ Z.cursorPosition $ i ^. editorContentL

renderDebugInfos i =
  let debugInfos = hBox [
         vBox [
         renderBPM i,
         renderTick i],
         padLeft (Pad 10) $ vBox [
         renderCursorPosition i]
         ]
  in  debugInfos

renderCanvasEditor i =
   withAttr gridBgAttr $ vLimitPercent 80 $ hLimitPercent 80 $
     E.renderEditor (txt . T.unlines) True $ i ^. editor

drawApp :: BPMInfo -> [Widget UIComponentName]
drawApp f =  [center (vBox [
                   renderCanvasEditor f,
                   renderDebugInfos f
                    ])
            ]
gridArrayPattern :: Int -> (Int, Int) -> Int -> Char
gridArrayPattern g (w, h) i
        | (i + 1) `mod` w == 0 = '\n'
        | (x `mod` g == 0) && (y `mod` g == 0) = '+'
        | otherwise  = '.'
        where (x, y) = i `divMod` w

emptyArrayPattern :: (Int, Int) -> Int -> Char
emptyArrayPattern (w, h) i | (i + 1) `mod` w == 0 = '\n'
              | otherwise = ' '

textArray ::(Int, Int) -> ((Int, Int) -> Int -> Char) -> BoardArrayRowType
textArray (w, h) charAtPos =
  let
    bgPattern (c, r) i = Just (charAtPos (w, h) i, i + 1)

  in T.unfoldrN (h +(w * h)) (bgPattern (w, h)) 0


replaceChar :: Monoid t => Char -> Z.TextZipper t -> Z.TextZipper t
replaceChar c = Z.insertChar c . Z.deleteChar

deleteChar  :: (Eq t,Monoid t) => Z.TextZipper t -> Z.TextZipper t
deleteChar z =
  let
    (row, col) = Z.cursorPosition z
    c = ' ' --defaultCharAtPosForGridBgPattern (col, row) 100000000000
  in (Z.moveLeft. Z.deleteChar . Z.insertChar c) z

deletePrevChar :: (Eq t,Monoid t) => Z.TextZipper t -> Z.TextZipper t
deletePrevChar z  =
  let
    (row, col) = Z.cursorPosition z
    c = ' ' --defaultCharAtPosForGridBgPattern (col - 1, row) 100000000000
  in (Z.moveLeft . Z.insertChar c . Z.deletePrevChar) z


switchTickBool :: BPMInfo -> EventM UIComponentName (Next BPMInfo)
switchTickBool f = continue $ f & tickBool %~ not

tickHideIndicator f = continue $ f & tickBool .~ False
tickShowIndicator f = continue $ f & tickBool .~ True

step = tickShowIndicator

bpmDelta = BPM 10
handleBPMValueEvent setterOperator s  =
  let
    newState = s & over bpm (setterOperator bpmDelta)
    bpmChan' = newState ^. bpmChan
  in suspendAndResume $ do
                  atomically $ writeTChan bpmChan' $ newState ^. bpm
                  return newState


myHandleEditorEvent :: (Eq t, Monoid t) => V.Event -> E.Editor t n -> EventM n (E.Editor t n)
myHandleEditorEvent (V.EvKey (V.KChar c) []) s | c /= '\t' = return $ E.applyEdit (replaceChar c) s
myHandleEditorEvent (V.EvKey V.KBS []) s = return $ E.applyEdit deletePrevChar s
myHandleEditorEvent (V.EvKey V.KDel []) s = return $ E.applyEdit deleteChar s
myHandleEditorEvent e  s =  E.handleEditorEvent e s

handleHorcaEditorEvent :: BrickEvent UIComponentName HorcaEvent -> BPMInfo -> EventM UIComponentName (Next BPMInfo)
handleHorcaEditorEvent (AppEvent TickEvent) s = step s
handleHorcaEditorEvent (AppEvent TickHideIndicator) s = tickHideIndicator s
handleHorcaEditorEvent (VtyEvent V.EvResize {})    s   = continue s
handleHorcaEditorEvent (VtyEvent (V.EvKey V.KEsc [])) s  = halt s

handleHorcaEditorEvent (VtyEvent (V.EvKey (V.KChar '-') [])) s = handleBPMValueEvent subtract s
handleHorcaEditorEvent (VtyEvent (V.EvKey (V.KChar '+') [])) s = handleBPMValueEvent (+) s

handleHorcaEditorEvent (VtyEvent e) s =
  do
    let exprVar' = s ^. exprVar
    let prev = s ^. editorContentL
    res <- handleEventLensed s editor myHandleEditorEvent e
    let new = res ^. editorContentL
    if Z.getText prev == Z.getText new
    then continue res
    else suspendAndResume $ do
                   atomically $ writeTChan exprVar' new
                   return res


horcaEditorApp :: App BPMInfo HorcaEvent UIComponentName
horcaEditorApp = let
    _attrMap = attrMap
  in
    App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = flip handleHorcaEditorEvent
    , appStartEvent = return
    , appAttrMap  = const theMap

    }

--editorTextWithBg :: n -> (Maybe Int) -> T.Text -> (Int -> Char) -> E.Editor n T.Text
--editorTextWithBg = E.editorText


uiLoop :: App BPMInfo HorcaEvent UIComponentName -> BC.BChan HorcaEvent -> BPMInfo -> IO BPMInfo
uiLoop app eventQueue bpmInfo =
  do
    initialVty <- buildVty
    customMain initialVty buildVty (Just eventQueue) app bpmInfo
  where
    buildVty =
      do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

tickWatcher :: TChan Tick -> IO (BC.BChan HorcaEvent, IO ())
tickWatcher ticks = do
    eventQueue <- BC.newBChan 2
    return (eventQueue, watcherLoop ticks eventQueue)
  where watcherLoop ticks eventQueue = forever $ do
          Tick <- atomically $ readTChan ticks
          BC.writeBChan eventQueue TickEvent
          threadDelay 50000
          BC.writeBChan eventQueue TickHideIndicator
          return ()

runBrickUI :: TChan BPM -> TChan Tick -> IO (TChan BoardArrayType, Async (), Async BPMInfo)
runBrickUI bpmVar ticks =
  do
    (initBPM, exprVar') <- atomically readBPMCreateExprVar

    let bpmInfo = BPMInfo {
         _bpm = initBPM,
         _bpmChan = bpmVar,
         _tickBool = False,
         _editor = E.editorText OpsCanvas (Just h) initEditorContent,
         _exprVar = exprVar'
         }

    readableTicks <- atomically $ dupTChan ticks
    (eventQueue, tickWatcherIO) <- tickWatcher readableTicks

    tickWatcherAsync <- async tickWatcherIO
    uiLoopAsync <- async $ uiLoop horcaEditorApp eventQueue bpmInfo
    waitEither tickWatcherAsync uiLoopAsync

    return (exprVar', tickWatcherAsync, uiLoopAsync)
  where
    w = 80
    h = 80
    initEditorContent = textArray (w, h) (gridArrayPattern 8)
    readBPMCreateExprVar =
      do
        initBpm <- readTChan bpmVar
        exprVar' <- newTChan
        return (initBpm, exprVar')

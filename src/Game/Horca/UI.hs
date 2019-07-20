{-# LANGUAGE TemplateHaskell #-}

module Game.Horca.UI where

import           Game.Horca.Types

import           Data.IORef

import qualified Brick as B --(App(..), Widget, attrMap, showFirstCursor, str)
import Graphics.Vty.Attributes (defAttr)

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Zipper              as Z

import Data.IORef

import           Lens.Micro
import           Lens.Micro.TH

import BroadcastChan as BC

initAppState :: BPM -> AppState
initAppState b =
  AppState {
    _bpm = b
  }

broadcastChanToBrickEvent = do
   tick <- BC.readBChan ticks
   B.writeBChan brickChannel $ TickEvent

-- tickWatcher :: BC.BroadcastChan BC.Out Tick -> IO B.BChan
-- tickWatcher ticks =
--   let
--     brickChannel = B.newBChan


--   in withAsync


--     do
--       a <- async (
--         forever $ do
--           tick <- BC.readBroadCastChan ticks
--           B.writeBChan brickChannel $ TickEvent
--         )
--       return brickChannel
    --   c <- readBChan chan
    --   writeIORef bpmRef c
  --  bpm <- readIORef bpmRef






   --  forever $
   -- do
   --  tick <- BC.readBroadCastChan ticks
   --  B.writeBChan brickChannel $ TickEvent
    --   c <- readBChan chan
    --   writeIORef bpmRef c
  --  bpm <- readIORef bpmRef


uiThread :: BroadcastChan In Tick -> (B.App AppState HorcaEvent Name) -> AppState -> IO ()
uiThread ticks app initState =
  do
    brickSink <- newBChan (initState ^. bpm)
    x <-
      (tickWatcher ticks brickSink)
      (B.customMain VTY.Default _ brickSink (app) initState) -- $ show bpm
    putStrLn "vfv"

module Game.Horca.TickTimer where

import Game.Horca.Types

import           Control.Arrow
import           Control.Monad (forever)
import           Control.Concurrent
import           Control.Concurrent.Async
import Control.Concurrent.STM (atomically, TChan)
import Control.Concurrent.STM.TChan (writeTChan, readTChan, dupTChan,
                                newBroadcastTChanIO, tryReadTChan)
import Control.Concurrent.STM.TVar (readTVar)

import           Data.Maybe (fromMaybe)



import           Data.Ratio as R


bpmControllableTickLoop ::  BPM -> TChan BPM -> TChan Tick -> IO ()
bpmControllableTickLoop prevBPM bpmChan tickChan =
   do
     atomically $ writeTChan tickChan Tick
     bpmMaybe <- atomically $ tryReadTChan bpmChan
     let bpm' = fromMaybe prevBPM bpmMaybe
     threadDelay $ bpmToMilliseconds bpm'
     bpmControllableTickLoop bpm' bpmChan tickChan

forkBPMComtrollableTickTimer :: TChan BPM -> IO (TChan Tick, Async ())
forkBPMComtrollableTickTimer bpmChan = do
                         tickChan <- newBroadcastTChanIO
                         asyncRes <- async $ bpmControllableTickLoop (BPM 60) bpmChan tickChan
                         return (tickChan, asyncRes)

bpmToMilliseconds :: BPM -> Int
bpmToMilliseconds (BPM bpm) =
  let
    sixtySecInMicro = (60 * 1000000)
  in
    sixtySecInMicro `div` (fromInteger bpm)

module Game.Horca.TickTimer where

import Game.Horca.Types


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically, TChan)
import Control.Concurrent.STM.TChan (newTChan, writeTChan, readTChan, dupTChan,
                                newBroadcastTChan, tryReadTChan)
import Control.Concurrent.STM.TVar (readTVar)

import Data.Maybe (fromMaybe)

bpmControllableTickLoop ::  BPM -> TChan BPM -> TChan Tick -> IO ()
bpmControllableTickLoop prevBPM bpmChan tickChan =
   do
     bpmMaybe <- atomically $ do
       writeTChan tickChan Tick
       tryReadTChan bpmChan       

     let bpm' = fromMaybe prevBPM bpmMaybe
     threadDelay $ bpmToMilliseconds bpm'
     bpmControllableTickLoop bpm' bpmChan tickChan
  where bpmToMilliseconds (BPM bpm) = 60000000 `div` (fromInteger bpm)

forkBPMComtrollableTickTimer :: BPM -> IO (TChan BPM, TChan Tick, Async ())
forkBPMComtrollableTickTimer initBPM = do
  (bpmChan, tickChan) <- atomically $ do
    b <- newTChan
    writeTChan b initBPM
    tickChan <- newBroadcastTChan
    return (b, tickChan)  
                         
  asyncRes <- async $ bpmControllableTickLoop initBPM bpmChan tickChan
  return (bpmChan, tickChan, asyncRes)


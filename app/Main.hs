module Main where

import           Game.Horca.Types
import qualified Game.Horca.UI.BPMForm as F
import           Game.Horca.TickTimer

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (TChan, writeTChan, newTChan, readTChan)

import qualified  Data.Text.Zipper as Z

import Control.Monad (forever)

interprete :: (Show t, Monoid t) => TChan (Z.TextZipper t) -> IO ()
interprete zVar =  forever $
  do
    z' <- atomically $  readTChan zVar
    putStrLn $ show $ Z.getText z'



--run :: ActionBanana -> Chan Midi


main :: IO ()
main = do

  bpmVar <- atomically $ do
    b <- newTChan
    writeTChan b (BPM 120)
    return b
 -- exprVar' <- newTVar (Z.textZipper [T.empty] (Just 1))


  (ticksChan, tickTimerAsync) <- forkBPMComtrollableTickTimer bpmVar
  (exprVar', watcherAsync, uicAsync) <- F.runBrickUI bpmVar ticksChan
  expr <- async $ interprete exprVar'
  --() <- run expr

  finalBpm <- waitEither uicAsync tickTimerAsync

  cbpm <- atomically $  readTChan bpmVar
  putStrLn $ show cbpm

  return ()

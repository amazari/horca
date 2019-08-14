module Main where

import           Game.Horca.Types
import qualified Game.Horca.UI.Brick.Editor as F
import           Game.Horca.TickTimer

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (TChan, writeTChan, newTChan, readTChan)


import Control.Monad (forever)

-- interprete :: (Show t, Monoid t) => TChan (Z.TextZipper t) -> IO ()
-- interprete zVar =  forever $
--   do
--     z' <- atomically $  readTChan zVar
--     putStrLn $ show $ Z.getText z'



--run :: ActionBanana -> Chan Midi


main :: IO ()
main = do

 -- exprVar' <- newTVar (Z.textZipper [T.empty] (Just 1))


  (bpmChan, ticksChan, tickTimerAsync) <- forkBPMComtrollableTickTimer (BPM 120)
  (exprVar', watcherAsync, uicAsync) <- F.runBrickUI bpmChan ticksChan
 -- expr <- async $ interprete exprVar'
  --() <- run expr

  finalBpm <- waitEither uicAsync tickTimerAsync

  cbpm <- atomically $  readTChan bpmChan
  putStrLn $ show cbpm

  return ()

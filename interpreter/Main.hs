{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Game.Horca.Types
import Game.Horca.TickTimer

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Concurrent.STM (atomically, TChan, readTChan, dupTChan)

import Control.Monad (forever)




--counter :: Int -> Int -> Behavior Int
-- counter from to = accumB from $ _


addB :: Behavior Int -> Behavior Int -> Behavior Int
addB i j = (+) <$> i <*> j





bounce from to (i, step) =
  let stepI = if i + step > to || i + step < from then -step else step
  in (i + stepI, stepI)

bb :: MonadMoment m => Event x -> Int -> Int -> Int -> m (Behavior (Int, Int))
bb e from' to' step' = accumB (from', step') $ (bounce from' to') <$ e

bounceB from to step e = let f from' to' step' = accumE (from', step') $ (bounce from' to') <$ e
             in f <$> from <*> to <*> step


makeNetworkDescription ::
                       AddHandler Tick
                       -> MomentIO ()
makeNetworkDescription addKeyEvent = do
    tickEvent <- fromAddHandler addKeyEvent

    res <- accumE 0 $  (\c -> c+1) <$ tickEvent


    let (zero, one, two, ten) :: (Behavior Int, Behavior Int, Behavior Int, Behavior Int) = (pure 0, pure 1, pure 2, pure 10 )

    
    bouncingB :: Behavior (Integer, Integer) <- accumB (42, 1) $ (bounce 42 50) <$ tickEvent
    bouncing <- changes bouncingB -- :: Event (Integer, Integer) <- accumE (42, 1) $ (bounce 42 50) <$ tickEvent   
    reactimate' $ fmap (print.fst)  <$> bouncing

    
    -- let
    --   isAfter10Ticks = fmap (\i -> if i > 9 then a else b) res
    -- z <- (switchB b isAfter10Ticks)
    -- let d = addB a z <@ tickEvent
    -- reactimate $ fmap print d



    -- let incr = (\c -> c+1) <$ tickEvent
  
  --  reactimate $ fmap print res



-- registerTickEvent :: TChan Tick -> IO (AddHandler Tick)
-- registerTickEvent ticks = do
--     (addTickEvent, fireTick) <- newAddHandler
--     tick <- atomically $ readTChan ticks
--     return (addTickEvent)



main = do

  (_, ticksChan, tickTimerAsync) <- forkBPMComtrollableTickTimer (BPM 120)


  (addTickEvent, fireTick) <- newAddHandler
  network <- compile (makeNetworkDescription addTickEvent)
  actuate network

  readableTicks <- atomically $ dupTChan ticksChan
  forever $ do
    tick <- atomically $ readTChan readableTicks
    fireTick tick

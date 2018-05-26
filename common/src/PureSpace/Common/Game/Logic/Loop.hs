--Loop.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE MultiParamTypeClasses #-}

module PureSpace.Common.Game.Logic.Loop
  (
    logicLoop,
    runLogic,
    Timer (..)
  )
  where

import           PureSpace.Common.Concurrent
import           PureSpace.Common.Lens
import           System.Clock
import           PureSpace.Common.Game.Entity
import           PureSpace.Common.Game.GameFSM
import           PureSpace.Common.Prelude

type Milliseconds = Integer
type Microseconds = Integer
type Nanoseconds  = Integer

newtype Timer = Timer Integer

class HasTime t where
  time :: Lens' t Integer

instance HasTime Timer where
  time =
    let f (Timer u) = u
        g _ = Timer
    in lens f g

data LoopState = LoopState Timer GameState

instance HasGameState LoopState where
  gameState =
    let f (LoopState _ b) = b
        g (LoopState a _) = LoopState a
    in lens f g

instance HasTime LoopState where
  time =
    let f (LoopState a _) = a ^. time
        g (LoopState _ b) a = LoopState (Timer a) b
    in lens f g

initialLoopState :: (MonadIO m) => GameConfig -> m LoopState
initialLoopState config =
  liftIO $ LoopState <$> (Timer . toNanoSecs <$> getTime MonotonicRaw)
                     <*> (pure . initial $ config)

runLogic :: (MonadIO m) => TChan GameState -> GameConfig -> m ()
runLogic sink config = do
  firstState <- initialLoopState config
  runReaderT (evalStateT (logicLoop sink) firstState) config

logicLoop :: (MonadIO m,
              MonadState s m,
              MonadReader r m,
              HasTime s,
              HasGameState s,
              HasGridSize r,
              HasGridDivision r,
              HasGameConfig r) => TChan GameState -> m ()
logicLoop sink = do
    tick
    newState <- use gameState
    liftIO $ atomically (writeTChan sink newState)
    liftIO (threadDelay . fromIntegral . milliToMicro $ 50)
    logicLoop sink

initial :: GameConfig -> GameState
initial (GameConfig a b) =
  GameState (createSpatialGrid a b (EntityShip <$> oneShips <> twoShips))
    [PlayerState One [] oneShips 0 [],
     PlayerState Two [] twoShips 0 []]
  where
    pc = ProjectileCaracteristics (ProjectileType Laser 2 6) 10 (V2 1500 1500)
    sc = ShipCaracteristics (ShipType Fighter 100 75) pc 100 (V2 300 300) 1 (CircleRange 500)
    oneShips = [Ship sc One 100 0 (V2 0 1000) (V2 0 0) 0,
                Ship sc One 100 0 (V2 0 500) (V2 0 0) 0,
                Ship sc One 100 0 (V2 0 0) (V2 0 0) 0]
    twoShips = [Ship sc Two 100 0 (V2 1500 1000) (V2 0 0) 0,
                Ship sc Two 100 0 (V2 1500 500) (V2 0 0) 0,
                Ship sc Two 100 0 (V2 1500 0) (V2 0 0) 0]

nanoToMilli :: Nanoseconds -> Milliseconds
nanoToMilli = (`div` 1000000)

milliToMicro :: Milliseconds -> Microseconds
milliToMicro = (* 1000)

tick :: (MonadIO m,
         MonadState s m,
         MonadReader r m,
         HasTime s,
         HasGameState s,
         HasGridSize r,
         HasGridDivision r,
         HasGameConfig r)
     => m ()
tick = do
  previous <- get
  now <- liftIO $ toNanoSecs <$> getTime MonotonicRaw
  let delta = nanoToMilli $ now - (previous ^. time)
  modify $ time .~ now
  let ellapsedSeconds = fromInteger delta / 1000
  liftIO $ print ellapsedSeconds
  config <- ask
  prevGameState <- use gameState
  newState <- pure $ execState (runReaderT (updateGame ellapsedSeconds) config) prevGameState
  gameState .= newState

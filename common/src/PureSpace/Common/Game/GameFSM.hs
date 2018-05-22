-- GameFSM.hs ---

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
{-# LANGUAGE RankNTypes #-}

module PureSpace.Common.Game.GameFSM
  (
    module PureSpace.Common.Game.Collision,
    module PureSpace.Common.Game.Geometry,
    module PureSpace.Common.Game.State,
    updateGame
  )
  where

import           Linear
import           PureSpace.Common.Game.Collision
import           PureSpace.Common.Game.Geometry
import           PureSpace.Common.Game.State
import           PureSpace.Common.Game.Entity
import           PureSpace.Common.Lens

{-
TODO: step units
TODO: collision check
TODO: AI trajectory
-}

updateGame :: (MonadState s m, HasGameState s, HasPlayers s) => DeltaTime -> m ()
updateGame dt = do
  gameState %= updatePlayers dt
  grid <- createSpatialGrid 800 20 <$> getEntities
  updateShipsObjective grid

updateShipsObjective :: (MonadState s m, HasPlayers s) => Grid Entity -> m ()
updateShipsObjective (Grid s d) = do
  -- TODO: for each ship, if any enemy is in range, stop moving and shot at him
  -- TODO: otherwise, move to the nearest enemy base
  pure ()

enemyTeam :: Team -> (Team -> Bool)
enemyTeam = (/=)

allyTeam :: Team -> (Team -> Bool)
allyTeam = (==)

getBases :: (MonadState s m, HasPlayers s) => m [Base]
getBases = (view bases =<<) <$> use players

getShips :: (MonadState s m, HasPlayers s) => m [Ship]
getShips = (view ships =<<) <$> use players

getEntities :: (MonadState s m, HasPlayers s) => m [Entity]
getEntities = do
  s <- fmap EntityShip <$> getShips
  b <- fmap EntityBase <$> getBases
  pure $ s ++ b

getTeamEntities :: (MonadState s m, HasPlayers s) => Team -> m [Entity]
getTeamEntities t = do
  s <- fmap EntityShip . ofTeam <$> getShips
  b <- fmap EntityBase . ofTeam <$> getBases
  pure $ s ++ b
  where
    ofTeam :: (HasTeam s) => [s] -> [s]
    ofTeam = filter ((==) t . view team)

updatePlayers :: (HasPlayers s) => DeltaTime -> s -> s
updatePlayers dt = players %~ fmap (updatePlayer dt)

updatePlayer :: DeltaTime -> PlayerState -> PlayerState
updatePlayer dt x = x & (cash +~ incomeDt) . updateShips dt
  where
    incomeDt = let incomePerSecond = sum $ view income <$> x ^. bases
               in round $ fromIntegral incomePerSecond / dt

updateShips :: (HasShips s) => DeltaTime -> s -> s
updateShips dt = ships %~ fmap (updatePosition dt)

updatePosition :: (HasPosition s, HasVelocity s) => DeltaTime -> s -> s
updatePosition dt x = x & position +~ v ^* dt
  where
    v = x ^. velocity



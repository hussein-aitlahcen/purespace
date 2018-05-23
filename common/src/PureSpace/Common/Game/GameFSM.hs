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

module PureSpace.Common.Game.GameFSM
  (
    module PureSpace.Common.Game.Types,
    module PureSpace.Common.Game.Collision,
    module PureSpace.Common.Game.Geometry,
    module PureSpace.Common.Game.State,
    updateGame
  )
  where

import qualified Data.Set                        as S
import qualified Data.Vector                     as V
import           Linear
import           PureSpace.Common.Game.Collision
import           PureSpace.Common.Game.Entity
import           PureSpace.Common.Game.Geometry
import           PureSpace.Common.Game.State
import           PureSpace.Common.Game.Types
import           PureSpace.Common.Lens
import           PureSpace.Common.Prelude

{-
  TODO: game config (map width etc...)
  TODO: step units
  TODO: collision check
  TODO: AI trajectory
  -- TODO: for each ship, if any enemy is in range, stop moving and shot at him
  -- TODO: otherwise, move to the nearest enemy base
-}

updateGame :: (MonadState s m, HasGameState s, HasPlayers s) => DeltaTime -> m (Grid Entity)
updateGame dt = do
  grid <- createSpatialGrid 1000 20 <$> getEntities
  gameState %= updatePlayers dt grid
  pure grid

enemyTeamOf :: HasTeam s => Team -> (s -> Bool)
enemyTeamOf t = (/=) t . view team

allyTeamOf :: HasTeam s => Team -> (s -> Bool)
allyTeamOf t = (==) t . view team

getBases :: (MonadState s m, HasPlayers s) => m [Base]
getBases = (view bases =<<) <$> use players

getShips :: (MonadState s m, HasPlayers s) => m [Ship]
getShips = (view ships =<<) <$> use players

getProjectiles :: (MonadState s m, HasPlayers s) => m [Projectile]
getProjectiles = (view projectiles =<<) <$> use players

getEntities :: (MonadState s m, HasPlayers s) => m [Entity]
getEntities = do
  s <- fmap EntityShip       <$> getShips
  b <- fmap EntityBase       <$> getBases
  p <- fmap EntityProjectile <$> getProjectiles
  pure $ s <> b <> p

getTeamEntities :: (MonadState s m, HasPlayers s) => Team -> m [Entity]
getTeamEntities t = do
  s <- fmap EntityShip       . ofTeam <$> getShips
  b <- fmap EntityBase       . ofTeam <$> getBases
  p <- fmap EntityProjectile . ofTeam <$> getProjectiles
  pure $ s <> b <> p
  where
    ofTeam :: (HasTeam s) => [s] -> [s]
    ofTeam = filter (allyTeamOf t)

updatePlayers :: (HasPlayers s) => DeltaTime -> Grid Entity -> s -> s
updatePlayers dt grid = players %~ fmap (updatePlayer dt grid)

updatePlayer :: DeltaTime -> Grid Entity -> PlayerState -> PlayerState
updatePlayer dt grid x = x & updateShips dt grid . (cash +~ incomeDt)
  where
    incomeDt = let incomePerSecond = sum $ view income <$> x ^. bases
               in round $ fromIntegral incomePerSecond / dt

updateShips :: (HasShips s) => DeltaTime -> Grid Entity -> s -> s
updateShips dt grid = ships %~ fmap (updatePosition dt . updateObjective grid)

updatePosition :: (HasPosition s, HasVelocity s) => DeltaTime -> s -> s
updatePosition dt x = x & position +~ v ^* dt
  where
    v = x ^. velocity

updateObjective :: Grid Entity -> Ship -> Ship
updateObjective grid s@(Ship _ t _ _ _ _) =
  case enemyInRange of
    Just (EntityShip enemy)                 -> resetVelocity -- TODO: fire
    Just (EntityBase enemyBase)             -> resetVelocity -- TODO: fire aswell ?
    Just (EntityProjectile enemyProjectile) -> resetVelocity -- TODO: depending on the ship, destroy it ?
    Nothing -> case nearestEnemy of
      Just (EntityShip enemy)     -> s & velocity .~ normalize (direction (s ^. position) (enemy ^. position)) * (s ^. maxVelocity)
      Just (EntityBase enemyBase) -> resetVelocity -- TODO: fire
      Nothing                     -> s             -- TODO: should be the end of the game

  where

    resetVelocity = s & velocity .~ V2 0 0

    -- TODO: nearest please
    enemyInRange =
      let enemiesInRange =
            let inRange     = computeRange grid (EntityShip s) (s ^. fireRange)
                reduction   = V.foldr' shipsOnly V.empty
                enemiesOnly = V.filter (enemyTeamOf t) . reduction
            in enemiesOnly inRange
          shipsOnly entity acc = V.cons entity acc
      in enemiesInRange V.!? 0

    -- TODO: TOTAL function please
    nearestEnemy =
      let enemies = S.filter (enemyTeamOf t) (eliminateSpatialGrid grid)
      in safeHead $ S.toList enemies

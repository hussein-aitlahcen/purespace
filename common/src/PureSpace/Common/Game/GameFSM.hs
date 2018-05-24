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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
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

import qualified Data.PQueue.Prio.Min            as PQ
import qualified Data.Set                        as S
import qualified Data.Vector                     as V
import           Linear
import           PureSpace.Common.Game.Action
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

type GameActionWriter m = MonadWriter [GameAction] m

updateGame :: (MonadState s m,
               HasGameState s,
               HasPlayers s) => DeltaTime -> m (Grid Entity)
updateGame dt = do
  grid <- createSpatialGrid 3000 20 <$> getEntities
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

updatePlayers :: (HasPlayers s)
              => DeltaTime
              -> Grid Entity
              -> s
              -> s
updatePlayers dt grid = players %~ fmap (updatePlayer dt grid)

updatePlayer :: DeltaTime
             -> Grid Entity
             -> PlayerState
             -> PlayerState
updatePlayer dt grid player =
  let (nextPlayer, actions) = runWriter $ updateShips dt grid player
      endoActions           = executeAction <$> actions
      composedActions       = mconcat endoActions
  in appEndo composedActions . updateProjectiles dt . updateCash $ nextPlayer

  where
    updateCash :: (HasCash s, HasBases s) => s -> s
    updateCash s =
      let incomeDt =
            let incomePerSecond = sum $ view income <$> s ^. bases
            in round $ fromIntegral incomePerSecond / dt
      in s & cash +~ incomeDt

    executeAction :: GameAction -> Endo PlayerState
    executeAction (ShotTarget a b) = Endo go
      where
        go =
          let shotPosition   = a ^. position
              targetPosition = b ^. position
              projCarac      = a ^. projectileCaracteristics
              projMaxV       = projCarac ^. maxVelocity
              newProj        = Projectile projCarac (a ^. team) shotPosition (signorm (direction shotPosition targetPosition) * projMaxV)
          in projectiles %~ (:) newProj

updateShips :: (GameActionWriter m,
                HasShips s)
            => DeltaTime
            -> Grid Entity
            -> s
            -> m s
updateShips dt grid st =
  (st &) . (ships .~) <$> traverse (fmap (updatePosition dt) <$> updateShipObjective dt grid) (st ^. ships)

updateProjectiles :: (HasProjectiles s) => DeltaTime -> s -> s
updateProjectiles dt = projectiles %~ fmap (updatePosition dt)

updatePosition :: (HasPosition s, HasVelocity s) => DeltaTime -> s -> s
updatePosition dt entity =
  let v = entity ^. velocity
  in entity & position +~ v ^* dt

updateShipObjective :: GameActionWriter m
                    => DeltaTime
                    -> Grid Entity
                    -> Ship
                    -> m Ship
updateShipObjective dt grid s@(Ship _ t _ _ _ _) =
  let resetFireCooldown = fireCooldown .~ (1 / s ^. fireRate)
      lowerFireCooldown = fireCooldown -~ dt
      resetVelocity     = velocity .~ V2 0 0
  in case enemyInRange of
       Just (EntityShip enemy) ->
         if s ^. fireCooldown <= 0
         then do
           tell [ShotTarget s enemy]
           pure $ s & resetFireCooldown . resetVelocity
         else
           pure $ s & lowerFireCooldown . resetVelocity

       Just (EntityBase enemyBase)             -> pure $ s & resetVelocity -- TODO: fire aswell ?

       Just (EntityProjectile enemyProjectile) -> pure s -- TODO: nothing

       Nothing -> case nearestEnemy of
         Just (EntityShip enemy) ->
           let pos      = s     ^. position
               enemyPos = enemy ^. position
               maxV     = s     ^. maxVelocity
           in pure $ s & velocity .~ signorm (direction pos enemyPos) * maxV

         Just (EntityBase enemyBase)             -> pure $ s & resetVelocity -- TODO: fire

         Just (EntityProjectile enemyProjectile) -> pure $ s & resetVelocity -- TODO: nothing

         Nothing                                 -> pure $ s & resetVelocity -- TODO: should be the end of the game

  where

    -- TODO: nearest ship/base, nothing else
    enemyInRange :: Maybe Entity
    enemyInRange =
      let enemiesInRange =
            let inRange                  = computeRange grid (EntityShip s) (s ^. fireRange)
                shipsOnly (EntityShip _) = True
                shipsOnly _              = False
                enemiesOnly              = PQ.filter (liftA2 (&&) shipsOnly (enemyTeamOf t))
            in enemiesOnly inRange
      in snd <$> PQ.getMin enemiesInRange

    -- TODO: nearest ship/base, nothing else
    nearestEnemy :: Maybe Entity
    nearestEnemy =
      let enemies = S.filter (enemyTeamOf t) (eliminateSpatialGrid grid)
      in safeHead $ S.toList enemies

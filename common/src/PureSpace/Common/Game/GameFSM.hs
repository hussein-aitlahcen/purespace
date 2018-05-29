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
    module PureSpace.Common.Game.Entity,
    module PureSpace.Common.Game.Config,
    module PureSpace.Common.Game.State,
    updateGame
  )
  where

import qualified Data.PQueue.Prio.Min            as PQ
import           PureSpace.Common.Game.Action
import           PureSpace.Common.Game.Collision
import           PureSpace.Common.Game.Config
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
               MonadReader r m,
               HasGameState s,
               HasSpatialGrid s,
               HasPlayers s,
               HasEntities s,
               HasNextObjectId s,
               HasGameConfig r,
               HasGridSize r,
               HasGridDivision r)
           => DeltaTime
           -> m ()
updateGame dt = updatePlayers dt *> updateEntities dt *> updateSpatialGrid

updateSpatialGrid :: (MonadState s m,
                      HasGameState s,
                      HasEntities s,
                      HasSpatialGrid s)
                  => m ()
updateSpatialGrid = do
  (Grid gs gd _ _ ) <- use spatialGrid
  nextGrid <- createSpatialGrid gs gd <$> use entities
  spatialGrid .= nextGrid

enemyTeamOf :: HasTeam s => Team -> (s -> Bool)
enemyTeamOf t = (/=) t . view team

allyTeamOf :: HasTeam s => Team -> (s -> Bool)
allyTeamOf t = (==) t . view team

getBases :: (MonadState s m, HasEntities s) => m [Base]
getBases =
  let step (EntityBase x) acc = x:acc
      step _ acc              = acc
  in foldr step [] <$> use entities

getShips :: (MonadState s m, HasEntities s) => m [Ship]
getShips =
  let step (EntityShip x) acc = x:acc
      step _ acc              = acc
  in foldr step [] <$> use entities

getProjectiles :: (MonadState s m, HasEntities s) => m [Projectile]
getProjectiles =
  let step (EntityProjectile x) acc = x:acc
      step _ acc                    = acc
  in foldr step [] <$> use entities

enemiesInRange :: (HasPosition a, HasTeam a) => Grid Entity -> a -> RangeType -> PQ.MinPQueue Distance Entity
enemiesInRange grid entity range =
  let inRange                  = computeRange grid entity range
      shipsOnly (EntityShip _) = True
      shipsOnly _              = False
      enemyShipsOnly           = liftA2 (&&) shipsOnly (enemyTeamOf (entity ^. team))
  in inRange enemyShipsOnly

nearestEnemy :: (HasPosition a, HasTeam a) => Grid Entity -> a -> RangeType -> Maybe Entity
nearestEnemy grid entity range =
  let enemies = enemiesInRange grid entity range
  in snd <$> PQ.getMin enemies

updatePlayers :: (MonadState s m, HasPlayers s, HasEntities s) => DeltaTime -> m ()
updatePlayers dt = do
  let playerBases p = filter ((== p ^. playerId) . view playerId) <$> getBases
      update      p = (p &) . updatePlayer dt <$> playerBases p
  players' <- traverse update =<< use players
  players .= players'

updatePlayer :: (HasCash a, HasIncome b) => DeltaTime -> [b] -> a -> a
updatePlayer dt incomeSources =
  let totalIncomePerSecond = sum     $ view income <$> incomeSources
      currentIncome        = round $ fromIntegral totalIncomePerSecond / dt
  in cash +~ currentIncome

updateEntities :: (MonadState s m,
                   HasEntities s,
                   HasNextObjectId s,
                   HasSpatialGrid s)
               => DeltaTime
               -> m ()
updateEntities dt = do
  grid <- use spatialGrid
  e    <- use entities
  let (actions, entities') = traverse (updateEntity dt grid) e
  entities'' <- foldrM executeGameAction entities' actions
  entities .= entities''

executeGameAction :: (MonadState s m,
                      HasNextObjectId s)
                  => GameAction
                  -> [Entity]
                  -> m [Entity]
executeGameAction (ShotTarget a b) e = do
  nextId  <- use nextObjectId
  let shotPosition   = a ^. position
      targetPosition = b ^. position
      projCarac      = a ^. projectileCaracteristics
      projMaxV       = projCarac ^. maxVelocity
      dir            = normalize (direction shotPosition targetPosition) * projMaxV
      phi              = directionAngle dir 0
      newProj        = EntityProjectile $ Projectile projCarac (a ^. team) shotPosition dir phi nextId 0
  pure $ newProj:e

updateEntity :: GameActionWriter m
             => DeltaTime
             -> Grid Entity
             -> Entity
             -> m Entity
updateEntity dt grid (EntityShip s)    = EntityShip . updatePosition dt <$> updateShipObjective dt grid s
updateEntity dt _ (EntityProjectile p) = pure $ EntityProjectile $ updatePosition dt p
updateEntity _ _ e                     = pure e

updatePosition :: (HasPosition s, HasVelocity s) => DeltaTime -> s -> s
updatePosition dt entity = entity & position +~ (entity ^. velocity) ^* dt

updateVelocity :: (HasPosition s, HasVelocity s, HasAngle s) => Velocity -> s -> s
updateVelocity v = (velocity .~ v) . (angle %~ directionAngle v)

updateShipObjective :: GameActionWriter m
                    => DeltaTime
                    -> Grid Entity
                    -> Ship
                    -> m Ship
updateShipObjective dt grid s =
  let resetFireCooldown  = fireCooldown .~ (1 / s ^. fireRate)
      reduceFireCooldown = fireCooldown -~ dt
      resetVelocity      = velocity .~ V2 0 0
      fireEnemy e        =
        let pos         = s ^. position
            enemyPos    = e ^. position
            updateAngle = angle %~ directionAngle (direction pos enemyPos)
        in updateAngle . resetFireCooldown . resetVelocity
      shipEntity = EntityShip s
  in
    case nearestEnemy grid shipEntity (s ^. rangeType) of
       Just (EntityShip enemy) ->
         bool (s ^. fireCooldown <= 0)
           (do
               tell [ShotTarget s enemy]
               pure $ s & fireEnemy enemy)
           (pure $ s & reduceFireCooldown . resetVelocity)
       Just (EntityBase enemyBase)             -> pure $ s & resetVelocity -- TODO: fire aswell ?
       Just (EntityProjectile enemyProjectile) -> pure s -- TODO: nothing
       Nothing ->
         case nearestEnemy grid shipEntity InfiniteRange of
           Just (EntityShip enemy) ->
             let pos      = s     ^. position
                 maxV     = s     ^. maxVelocity
                 enemyPos = enemy ^. position
                 v        = normalize (direction pos enemyPos) * maxV
             in pure $ updateVelocity v s
           Just (EntityBase enemyBase)             -> pure $ s & resetVelocity -- TODO: fire
           Just (EntityProjectile enemyProjectile) -> pure $ s & resetVelocity -- TODO: nothing
           Nothing                                 -> pure $ s & resetVelocity -- TODO: should be the end of the game

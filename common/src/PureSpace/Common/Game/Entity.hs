-- Entity.hs ---
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
module PureSpace.Common.Game.Entity
  ( module PureSpace.Common.Game.Base
  , module PureSpace.Common.Game.Ship
  , Entity(..)
  , HasEntities(..)
  ) where

import PureSpace.Common.Game.Base
import PureSpace.Common.Game.Ship
import PureSpace.Common.Lens

data Entity
  = EntityShip Ship
  | EntityProjectile Projectile
  | EntityBase Base
  deriving (Eq, Ord, Show)

class HasEntities s where
  entities :: Lens' s [Entity]

instance HasPlayerId Entity where
  playerId =
    let f (EntityShip s) = s ^. playerId
        f (EntityProjectile proj) = proj ^. playerId
        f (EntityBase b) = b ^. playerId
        g (EntityShip s) pid = EntityShip $ s & playerId .~ pid
        g (EntityProjectile proj) pid =
          EntityProjectile $ proj & playerId .~ pid
        g (EntityBase b) pid = EntityBase $ b & playerId .~ pid
     in lens f g

instance HasObjectId Entity where
  objectId =
    let f (EntityShip s) = s ^. objectId
        f (EntityProjectile proj) = proj ^. objectId
        f (EntityBase b) = b ^. objectId
        g (EntityShip s) oid = EntityShip $ s & objectId .~ oid
        g (EntityProjectile proj) oid =
          EntityProjectile $ proj & objectId .~ oid
        g (EntityBase b) oid = EntityBase $ b & objectId .~ oid
     in lens f g

instance HasTeam Entity where
  team =
    let f (EntityShip s) = s ^. team
        f (EntityProjectile proj) = proj ^. team
        f (EntityBase b) = b ^. team
        g (EntityShip s) t = EntityShip $ s & team .~ t
        g (EntityProjectile proj) t = EntityProjectile $ proj & team .~ t
        g (EntityBase b) t = EntityBase $ b & team .~ t
     in lens f g

instance HasPosition Entity where
  position =
    let f (EntityShip s) = s ^. position
        f (EntityProjectile proj) = proj ^. position
        f (EntityBase b) = b ^. position
        g (EntityShip s) p = EntityShip $ s & position .~ p
        g (EntityProjectile proj) p = EntityProjectile $ proj & position .~ p
        g (EntityBase b) p = EntityBase $ b & position .~ p
     in lens f g

instance HasWidth Entity where
  width =
    let f (EntityShip s) = s ^. width
        f (EntityProjectile proj) = proj ^. width
        f (EntityBase b) = b ^. width
        g (EntityShip s) w = EntityShip $ s & width .~ w
        g (EntityProjectile proj) w = EntityProjectile $ proj & width .~ w
        g (EntityBase b) w = EntityBase $ b & width .~ w
     in lens f g

instance HasHeight Entity where
  height =
    let f (EntityShip s) = s ^. height
        f (EntityProjectile proj) = proj ^. height
        f (EntityBase b) = b ^. height
        g (EntityShip s) h = EntityShip $ s & height .~ h
        g (EntityProjectile proj) h = EntityProjectile $ proj & height .~ h
        g (EntityBase b) h = EntityBase $ b & height .~ h
     in lens f g

instance HasAngle Entity where
  angle =
    let f (EntityShip s) = s ^. angle
        f (EntityProjectile proj) = proj ^. angle
        f (EntityBase b) = b ^. angle
        g (EntityShip s) h = EntityShip $ s & angle .~ h
        g (EntityProjectile proj) h = EntityProjectile $ proj & angle .~ h
        g (EntityBase b) h = EntityBase $ b & angle .~ h
     in lens f g

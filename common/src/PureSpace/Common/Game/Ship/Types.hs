-- Types.hs ---

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

module PureSpace.Common.Game.Ship.Types
  (
    module PureSpace.Common.Game.Projectile.Types,
    module PureSpace.Common.Game.Types,
    ShipType (..),
    Ship (..),
    ShipCaracteristics (..),
    HasShip (..),
    HasShipCaracteristics (..),
  )
  where

import           PureSpace.Common.Game.Projectile.Types
import           PureSpace.Common.Game.Types
import           PureSpace.Common.Lens                  (Lens', lens)

data ShipType = Fighter
              | Bomber
              | Frigate
              | Corvette
              | Destroyer
              | Cruiser
              | SupportShip
              | BattleShip
              | MotherShip
              | BattleStar
              | BaseStar
              | Juggernaut
              deriving Show

data Ship               = Ship ShipCaracteristics FireCooldown Position Velocity deriving Show
data ShipCaracteristics = ShipCaracteristics ShipType Projectile Health FireRate MaxVelocity deriving Show

class HasShip s where
  ship :: Lens' s Ship

instance HasShip Ship where
  ship = id

class HasShipCaracteristics s where
  shipCaracteristics :: Lens' s ShipCaracteristics

instance HasShipCaracteristics ShipCaracteristics where
  shipCaracteristics = id

instance HasShipCaracteristics Ship where
  shipCaracteristics =
    let f (Ship a _ _ _)   = a
        g (Ship _ b c d) a = Ship a b c d
    in lens f g

instance HasFireCooldown Ship where
  fireCooldown =
    let f (Ship _ b _ _)   = b
        g (Ship a _ c d) b = Ship a b c d
    in lens f g

instance HasPosition Ship where
  position =
    let f (Ship _ _ c _)   = c
        g (Ship a b _ d) c = Ship a b c d
    in lens f g

instance HasVelocity Ship where
  velocity =
    let f (Ship _ _ _ d)   = d
        g (Ship a b c _) d = Ship a b c d
    in lens f g

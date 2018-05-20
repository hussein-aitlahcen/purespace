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
    ShipType (..),
    Ship (..),
    ShipCaracteristics (..),
    HasShipType (..),
    HasShip (..),
    HasShipCaracteristics (..),
  )
  where

import           PureSpace.Common.Game.Projectile.Types
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

data Ship               = Ship ShipCaracteristics Health FireCooldown Position Velocity deriving Show
data ShipCaracteristics = ShipCaracteristics ShipType ProjectileCaracteristics MaxHealth MaxVelocity FireRate FireRange deriving Show

class HasShip s where
  ship :: Lens' s Ship

class HasShipCaracteristics s where
  shipCaracteristics :: Lens' s ShipCaracteristics

class HasShipType s where
  shipType :: Lens' s ShipType

instance HasShip Ship where
  ship = id

instance HasShipCaracteristics Ship where
  shipCaracteristics =
    let f (Ship a _ _ _ _)   = a
        g (Ship _ b c d e) a = Ship a b c d e
    in lens f g

instance HasHealth Ship where
  health =
    let f (Ship _ b _ _ _)   = b
        g (Ship a _ c d e) b = Ship a b c d e
    in lens f g

instance HasFireCooldown Ship where
  fireCooldown =
    let f (Ship _ _ c _ _)   = c
        g (Ship a b _ d e) c = Ship a b c d e
    in lens f g

instance HasPosition Ship where
  position =
    let f (Ship _ _ _ d _)   = d
        g (Ship a b c _ e) d = Ship a b c d e
    in lens f g

instance HasVelocity Ship where
  velocity =
    let f (Ship _ _ _ _ e)   = e
        g (Ship a b c d _) e = Ship a b c d e
    in lens f g

instance HasShipType Ship where
  shipType = shipCaracteristics . shipType

instance HasMaxHealth Ship where
  maxHealth = shipCaracteristics . maxHealth

instance HasMaxVelocity Ship where
  maxVelocity = shipCaracteristics . maxVelocity

instance HasFireRate Ship where
  fireRate = shipCaracteristics . fireRate

instance HasFireRange Ship where
  fireRange = shipCaracteristics . fireRange

instance HasProjectileCaracteristics Ship where
  projectileCaracteristics = shipCaracteristics . projectileCaracteristics

instance HasShipCaracteristics ShipCaracteristics where
  shipCaracteristics = id

instance HasShipType ShipCaracteristics where
  shipType =
    let f (ShipCaracteristics a _ _ _ _ _)   = a
        g (ShipCaracteristics _ b c d e f) a = ShipCaracteristics a b c d e f
    in lens f g

instance HasProjectileCaracteristics ShipCaracteristics where
  projectileCaracteristics =
    let f (ShipCaracteristics _ b _ _ _ _)   = b
        g (ShipCaracteristics a _ c d e f) b = ShipCaracteristics a b c d e f
    in lens f g

instance HasMaxHealth ShipCaracteristics where
  maxHealth =
    let f (ShipCaracteristics _ _ c _ _ _)   = c
        g (ShipCaracteristics a b _ d e f) c = ShipCaracteristics a b c d e f
    in lens f g

instance HasMaxVelocity ShipCaracteristics where
  maxVelocity =
    let f (ShipCaracteristics _ _ _ d _ _)   = d
        g (ShipCaracteristics a b c _ e f) d = ShipCaracteristics a b c d e f
    in lens f g

instance HasFireRate ShipCaracteristics where
  fireRate =
    let f (ShipCaracteristics _ _ _ _ e _)   = e
        g (ShipCaracteristics a b c d _ f) e = ShipCaracteristics a b c d e f
    in lens f g

instance HasFireRange ShipCaracteristics where
   fireRange =
    let f (ShipCaracteristics _ _ _ _ _ f)   = f
        g (ShipCaracteristics a b c d e _) f = ShipCaracteristics a b c d e f
    in lens f g

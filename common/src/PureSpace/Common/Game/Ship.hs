-- Ship.hs ---

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

module PureSpace.Common.Game.Ship
  (
    module PureSpace.Common.Game.Projectile,
    Ship (..),
    ShipCaracteristics (..),
    ShipType (..),
    ShipIdentifier (..),
    HasShip (..),
    HasShipCaracteristics (..),
    HasShipType (..),
    HasShipIdentifier (..),
  )
  where

import           PureSpace.Common.Game.Projectile
import           PureSpace.Common.Lens            (Lens', lens)

data Ship               = Ship ShipCaracteristics Team Health FireCooldown Position Velocity Angle                      deriving (Eq, Ord, Show)
data ShipCaracteristics = ShipCaracteristics ShipType ProjectileCaracteristics MaxHealth MaxVelocity FireRate RangeType deriving (Eq, Ord, Show)
data ShipType           = ShipType ShipIdentifier Width Height                                                          deriving (Eq, Ord, Show)
data ShipIdentifier     = Fighter
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
                        deriving (Eq, Ord, Show)

class HasShip s where
  ship :: Lens' s Ship

class HasShipCaracteristics s where
  shipCaracteristics :: Lens' s ShipCaracteristics

class HasShipType s where
  shipType :: Lens' s ShipType

class HasShipIdentifier s where
  shipIdentifier :: Lens' s ShipIdentifier

instance HasShip Ship where
  ship = id

instance HasShipCaracteristics Ship where
  shipCaracteristics =
    let f (Ship a _ _ _ _ _ _)   = a
        g (Ship _ b c d e k l) a = Ship a b c d e k l
    in lens f g

instance HasTeam Ship where
  team =
    let f (Ship _ b _ _ _ _ _)   = b
        g (Ship a _ c d e k l) b = Ship a b c d e k l
    in lens f g

instance HasHealth Ship where
  health =
    let f (Ship _ _ c _ _ _ _)   = c
        g (Ship a b _ d e k l) c = Ship a b c d e k l
    in lens f g

instance HasFireCooldown Ship where
  fireCooldown =
    let f (Ship _ _ _ d _ _ _)   = d
        g (Ship a b c _ e k l) d = Ship a b c d e k l
    in lens f g

instance HasPosition Ship where
  position =
    let f (Ship _ _ _ _ e _ _)   = e
        g (Ship a b c d _ k l) e = Ship a b c d e k l
    in lens f g

instance HasVelocity Ship where
  velocity =
    let f (Ship _ _ _ _ _ k _)   = k
        g (Ship a b c d e _ l) k = Ship a b c d e k l
    in lens f g

instance HasAngle Ship where
  angle =
    let f (Ship _ _ _ _ _ _ l)   = l
        g (Ship a b c d e k _) l = Ship a b c d e k l
    in lens f g

instance HasShipType Ship where
  shipType = shipCaracteristics . shipType

instance HasShipIdentifier Ship where
  shipIdentifier = shipCaracteristics . shipIdentifier

instance HasWidth Ship where
  width = shipCaracteristics . width

instance HasHeight Ship where
  height = shipCaracteristics . height

instance HasMaxHealth Ship where
  maxHealth = shipCaracteristics . maxHealth

instance HasMaxVelocity Ship where
  maxVelocity = shipCaracteristics . maxVelocity

instance HasFireRate Ship where
  fireRate = shipCaracteristics . fireRate

instance HasRangeType Ship where
  rangeType = shipCaracteristics . rangeType

instance HasProjectileCaracteristics Ship where
  projectileCaracteristics = shipCaracteristics . projectileCaracteristics

instance HasShipCaracteristics ShipCaracteristics where
  shipCaracteristics = id

instance HasShipType ShipCaracteristics where
  shipType =
    let f (ShipCaracteristics a _ _ _ _ _)   = a
        g (ShipCaracteristics _ b c d e k) a = ShipCaracteristics a b c d e k
    in lens f g

instance HasShipIdentifier ShipCaracteristics where
  shipIdentifier = shipType . shipIdentifier

instance HasWidth ShipCaracteristics where
  width = shipType . width

instance HasHeight ShipCaracteristics where
  height = shipType . height

instance HasProjectileCaracteristics ShipCaracteristics where
  projectileCaracteristics =
    let f (ShipCaracteristics _ b _ _ _ _)   = b
        g (ShipCaracteristics a _ c d e k) b = ShipCaracteristics a b c d e k
    in lens f g

instance HasMaxHealth ShipCaracteristics where
  maxHealth =
    let f (ShipCaracteristics _ _ c _ _ _)   = c
        g (ShipCaracteristics a b _ d e k) c = ShipCaracteristics a b c d e k
    in lens f g

instance HasMaxVelocity ShipCaracteristics where
  maxVelocity =
    let f (ShipCaracteristics _ _ _ d _ _)   = d
        g (ShipCaracteristics a b c _ e k) d = ShipCaracteristics a b c d e k
    in lens f g

instance HasFireRate ShipCaracteristics where
  fireRate =
    let f (ShipCaracteristics _ _ _ _ e _)   = e
        g (ShipCaracteristics a b c d _ k) e = ShipCaracteristics a b c d e k
    in lens f g

instance HasRangeType ShipCaracteristics where
   rangeType =
    let f (ShipCaracteristics _ _ _ _ _ k)   = k
        g (ShipCaracteristics a b c d e _) k = ShipCaracteristics a b c d e k
    in lens f g

instance HasShipType ShipType where
  shipType = id

instance HasShipIdentifier ShipType where
  shipIdentifier =
    let f (ShipType a _ _)   = a
        g (ShipType _ b c) a = ShipType a b c
    in lens f g

instance HasWidth ShipType where
  width =
    let f (ShipType _ b _)   = b
        g (ShipType a _ c) b = ShipType a b c
    in lens f g

instance HasHeight ShipType where
  height =
    let f (ShipType _ _ c)   = c
        g (ShipType a b _) c = ShipType a b c
    in lens f g

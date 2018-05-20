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

module PureSpace.Common.Game.Types
  (
    module Linear,
    Position,
    Velocity,
    MaxVelocity ,
    FireRate,
    FireRange,
    FireCooldown,
    Damage,
    Health,
    MaxHealth,
    HasPosition (..),
    HasVelocity (..),
    HasMaxVelocity (..),
    HasFireRate (..),
    HasFireRange (..),
    HasFireCooldown (..),
    HasDamage (..),
    HasHealth (..),
    HasMaxHealth (..),
  )
  where

import           Linear
import           PureSpace.Common.Lens (Lens')

{-
  FireRate     = per second
  Damage       = per projectile
  FireCooldown = next available shot
-}

type Position     = V2 Float
type Velocity     = V2 Float
type MaxVelocity  = Velocity
type FireRate     = Float
type FireCooldown = Float
type FireRange    = Float
type Damage       = Float
type Health       = Float
type MaxHealth    = Float

class HasPosition s where
  position :: Lens' s Position

class HasVelocity s where
  velocity :: Lens' s Velocity

class HasMaxVelocity s where
  maxVelocity :: Lens' s MaxVelocity

class HasFireRate s where
  fireRate :: Lens' s FireRate

class HasFireCooldown s where
  fireCooldown :: Lens' s FireCooldown

class HasFireRange s where
  fireRange :: Lens' s FireRange

class HasDamage s where
  damage :: Lens' s Damage

class HasHealth s where
  health :: Lens' s Health

class HasMaxHealth s where
  maxHealth :: Lens' s MaxHealth

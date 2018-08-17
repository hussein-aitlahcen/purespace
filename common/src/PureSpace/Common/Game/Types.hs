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
  ( V2(..)
  , PlayerId
  , ObjectId
  , Team(..)
  , RangeType(..)
  , Angle
  , Distance
  , DeltaTime
  , Position
  , Velocity
  , MaxVelocity
  , Direction
  , FireRate
  , FireCooldown
  , RespawnCooldown
  , Damage
  , Health
  , MaxHealth
  , Width
  , Height
  , ShipAmount
  , Cost
  , HasPlayerId(..)
  , HasObjectId(..)
  , HasTeam(..)
  , HasPosition(..)
  , HasVelocity(..)
  , HasMaxVelocity(..)
  , HasFireRate(..)
  , HasRangeType(..)
  , HasFireCooldown(..)
  , HasRespawnCooldown(..)
  , HasDamage(..)
  , HasHealth(..)
  , HasMaxHealth(..)
  , HasWidth(..)
  , HasHeight(..)
  , HasAngle(..)
  ) where

import Linear (V2(..))
import PureSpace.Common.Lens (Lens')
import PureSpace.Common.Prelude

{-
  FireRate     = per second
  Damage       = per projectile
  FireCooldown = next available shot
-}
type ObjectId = Int64

type PlayerId = ObjectId

type Angle = Float

type ShipAmount = Int

type Cost = Int

type Distance = Float

type Position = V2 Float

type Velocity = V2 Float

type MaxVelocity = Velocity

type Direction = V2 Float

type FireRate = Float

type FireCooldown = Float

type RespawnCooldown = Float

type Damage = Float

type Health = Float

type MaxHealth = Float

type Width = Int

type Height = Int

type DeltaTime = Float

data RangeType
  = InfiniteRange
  | CircleRange Float
  | RectangleRange (V2 Float)
  deriving (Eq, Ord, Show)

data Team
  = One
  | Two
  deriving (Eq, Ord, Show)

class HasObjectId t where
  objectId :: Lens' t ObjectId

class HasPlayerId t where
  playerId :: Lens' t PlayerId

class HasTeam t where
  team :: Lens' t Team

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

class HasRespawnCooldown s where
  respawnCooldown :: Lens' s RespawnCooldown

class HasRangeType s where
  rangeType :: Lens' s RangeType

class HasDamage s where
  damage :: Lens' s Damage

class HasHealth s where
  health :: Lens' s Health

class HasMaxHealth s where
  maxHealth :: Lens' s MaxHealth

class HasWidth s where
  width :: Lens' s Width

class HasHeight s where
  height :: Lens' s Height

class HasAngle s where
  angle :: Lens' s Angle

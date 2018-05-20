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

module PureSpace.Common.Game.Projectile.Types
  (
    module PureSpace.Common.Game.Types,
    ProjectileType (..),
    Projectile (..),
    ProjectileCaracteristics (..),
    HasProjectileType (..),
    HasProjectile (..),
    HasProjectileCaracteristics (..),
  )
  where

import           PureSpace.Common.Game.Types
import           PureSpace.Common.Lens       (Lens', lens)

data Projectile               = Projectile ProjectileCaracteristics Velocity               deriving Show
data ProjectileCaracteristics = ProjectileCaracteristics ProjectileType Damage MaxVelocity deriving Show
data ProjectileType           = ProjectileType ProjectileIdentifier Width Height           deriving Show
data ProjectileIdentifier     = Laser
                              | Rocket
                              deriving Show

class HasProjectile s where
  projectile :: Lens' s Projectile

class HasProjectileCaracteristics s where
  projectileCaracteristics :: Lens' s ProjectileCaracteristics

class HasProjectileType s where
  projectileType :: Lens' s ProjectileType

class HasProjectileIdentifier s where
  projectileIdentifier :: Lens' s ProjectileIdentifier

instance HasProjectile Projectile where
  projectile = id

instance HasProjectileCaracteristics Projectile where
  projectileCaracteristics =
    let f (Projectile a _)   = a
        g (Projectile _ b) a = Projectile a b
    in lens f g

instance HasVelocity Projectile where
  velocity =
    let f (Projectile _ b)   = b
        g (Projectile a _) b = Projectile a b
    in lens f g

instance HasProjectileType Projectile where
  projectileType = projectileCaracteristics . projectileType

instance HasProjectileIdentifier Projectile where
  projectileIdentifier = projectileCaracteristics . projectileIdentifier

instance HasWidth Projectile where
  width = projectileCaracteristics . width

instance HasHeight Projectile where
  height = projectileCaracteristics . height

instance HasDamage Projectile where
  damage = projectileCaracteristics . damage

instance HasMaxVelocity Projectile where
  maxVelocity = projectileCaracteristics . maxVelocity

instance HasProjectileCaracteristics ProjectileCaracteristics where
  projectileCaracteristics = id

instance HasProjectileType ProjectileCaracteristics where
  projectileType =
    let f (ProjectileCaracteristics a _ _)   = a
        g (ProjectileCaracteristics _ b c) a = ProjectileCaracteristics a b c
    in lens f g

instance HasProjectileIdentifier ProjectileCaracteristics where
  projectileIdentifier = projectileType . projectileIdentifier

instance HasWidth ProjectileCaracteristics where
  width = projectileType . width

instance HasHeight ProjectileCaracteristics where
  height = projectileType . height

instance HasDamage ProjectileCaracteristics where
  damage =
    let f (ProjectileCaracteristics _ b _)   = b
        g (ProjectileCaracteristics a _ c) b = ProjectileCaracteristics a b c
    in lens f g

instance HasMaxVelocity ProjectileCaracteristics where
  maxVelocity =
    let f (ProjectileCaracteristics _ _ c)   = c
        g (ProjectileCaracteristics a b _) c = ProjectileCaracteristics a b c
    in lens f g

instance HasProjectileType ProjectileType where
  projectileType = id

instance HasProjectileIdentifier ProjectileType where
  projectileIdentifier =
    let f (ProjectileType a _ _)   = a
        g (ProjectileType _ b c) a = ProjectileType a b c
    in lens f g

instance HasWidth ProjectileType where
  width =
    let f (ProjectileType _ b _)   = b
        g (ProjectileType a _ c) b = ProjectileType a b c
    in lens f g

instance HasHeight ProjectileType where
  height =
    let f (ProjectileType _ _ c)   = c
        g (ProjectileType a b _) c = ProjectileType a b c
    in lens f g

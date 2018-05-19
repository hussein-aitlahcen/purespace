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
    HasProjectileType (..),
    HasProjectile (..),
  )
  where

import           PureSpace.Common.Game.Types
import           PureSpace.Common.Lens       (Lens', lens)

data ProjectileType = Laser
                    | Rocket
                    deriving Show

data Projectile = Projectile ProjectileType Damage MaxVelocity deriving Show

class HasProjectile s where
  projectile :: Lens' s Projectile

instance HasProjectile Projectile where
  projectile = id

class HasProjectileType s where
  projectileType :: Lens' s ProjectileType

instance HasProjectileType Projectile where
  projectileType =
    let f (Projectile a _ _)   = a
        g (Projectile _ b c) a = Projectile a b c
    in lens f g

instance HasDamage Projectile where
  damage =
    let f (Projectile _ b _)   = b
        g (Projectile a _ c) b = Projectile a b c
    in lens f g

instance HasMaxVelocity Projectile where
  maxVelocity =
    let f (Projectile _ _ c)   = c
        g (Projectile a b _) c = Projectile a b c
    in lens f g

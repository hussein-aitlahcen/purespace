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
  (
    Entity (..),
  )
  where

import           PureSpace.Common.Game.Base
import           PureSpace.Common.Game.Ship
import           PureSpace.Common.Lens

data Entity = EntityShip       Ship
            | EntityProjectile Projectile
            | EntityBase       Base
            deriving Show

instance HasPosition Entity where
  position =
    let f (EntityShip       ship)   = ship ^. position
        f (EntityProjectile proj)   = proj ^. position
        f (EntityBase       base)   = base ^. position
        g (EntityShip       ship) p = EntityShip       $ ship & position .~ p
        g (EntityProjectile proj) p = EntityProjectile $ proj & position .~ p
        g (EntityBase       base) p = EntityBase       $ base & position .~ p
    in lens f g

instance HasWidth Entity where
  width =
    let f (EntityShip       ship)   = ship ^. width
        f (EntityProjectile proj)   = proj ^. width
        f (EntityBase       base)   = base ^. width
        g (EntityShip       ship) p = EntityShip       $ ship & width .~ p
        g (EntityProjectile proj) p = EntityProjectile $ proj & width .~ p
        g (EntityBase       base) p = EntityBase       $ base & width .~ p
    in lens f g

instance HasHeight Entity where
  height =
    let f (EntityShip       ship)   = ship ^. height
        f (EntityProjectile proj)   = proj ^. height
        f (EntityBase       base)   = base ^. height
        g (EntityShip       ship) p = EntityShip       $ ship & height .~ p
        g (EntityProjectile proj) p = EntityProjectile $ proj & height .~ p
        g (EntityBase       base) p = EntityBase       $ base & height .~ p
    in lens f g
